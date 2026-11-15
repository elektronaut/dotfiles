#!/usr/bin/env python3
"""PreToolUse hook for Bash: refuse git writes that run whether or not the steps before them succeeded.

A `git commit`, `git push` or `--continue` is refused when it follows `;`, a newline, `||` or `&`,
or when its `&&` chain contains a pipeline and `set -o pipefail` is not set. Commands this parser
does not understand are allowed.
"""

import json
import os
import sys

OPERATORS = ("&&", "||", ";;", "|&", ";", "|", "&", "\n", "(", ")")
KEYWORDS = {"if", "then", "else", "elif", "fi", "while", "until", "for", "do", "done", "case", "esac", "select", "function"}
COMPOUND = {"if", "while", "until", "for", "case", "select"}
COMPOUND_END = {"fi", "done", "esac"}
CONTINUE_SUBCOMMANDS = {"cherry-pick", "rebase", "merge", "revert", "am"}


class Unparsed(Exception):
    pass


def tokenize(src):
    tokens = []
    heredocs = []
    i, n = 0, len(src)
    word = None

    def flush():
        nonlocal word
        if word is not None:
            tokens.append(("w", word))
            word = None

    def skip_subst(j, close):
        depth = 1
        while j < n:
            c = src[j]
            if c == "\\":
                j += 2
                continue
            if c == "'" and close == ")":
                k = src.find("'", j + 1)
                if k < 0:
                    raise Unparsed
                j = k + 1
                continue
            if c == '"':
                j = skip_dquote(j + 1)
                continue
            if close == ")" and c == "(":
                depth += 1
            elif c == close:
                depth -= 1
                if depth == 0:
                    return j + 1
            j += 1
        raise Unparsed

    def skip_dquote(j):
        while j < n:
            c = src[j]
            if c == "\\":
                j += 2
                continue
            if c == '"':
                return j + 1
            if src.startswith("$(", j):
                j = skip_subst(j + 2, ")")
                continue
            if c == "`":
                j = skip_subst(j + 1, "`")
                continue
            j += 1
        raise Unparsed

    while i < n:
        c = src[i]
        if c == "\n":
            flush()
            tokens.append(("op", "\n"))
            i += 1
            for delim, strip_tabs in heredocs:
                while True:
                    end = src.find("\n", i)
                    line = src[i:] if end < 0 else src[i:end]
                    i = n if end < 0 else end + 1
                    if (line.lstrip("\t") if strip_tabs else line) == delim:
                        break
                    if end < 0:
                        break
            heredocs = []
            continue
        if c in " \t":
            flush()
            i += 1
            continue
        if c == "#" and word is None:
            end = src.find("\n", i)
            i = n if end < 0 else end
            continue
        if c == "\\":
            if i + 1 < n and src[i + 1] == "\n":
                i += 2
                continue
            word = (word or "") + src[i + 1 : i + 2]
            i += 2
            continue
        if c == "'":
            k = src.find("'", i + 1)
            if k < 0:
                raise Unparsed
            word = (word or "") + src[i + 1 : k]
            i = k + 1
            continue
        if c == '"':
            k = skip_dquote(i + 1)
            word = (word or "") + src[i + 1 : k - 1]
            i = k
            continue
        if src.startswith("$(", i):
            k = skip_subst(i + 2, ")")
            word = (word or "") + src[i:k]
            i = k
            continue
        if c == "`":
            k = skip_subst(i + 1, "`")
            word = (word or "") + src[i:k]
            i = k
            continue
        if src.startswith("<<", i) and not src.startswith("<<<", i):
            flush()
            j = i + 2
            strip_tabs = j < n and src[j] == "-"
            if strip_tabs:
                j += 1
            while j < n and src[j] in " \t":
                j += 1
            start = j
            while j < n and src[j] not in " \t\n;&|<>()":
                j += 1
            delim = src[start:j].replace("'", "").replace('"', "").replace("\\", "")
            if not delim:
                raise Unparsed
            heredocs.append((delim, strip_tabs))
            i = j
            continue
        if c in "<>":
            flush()
            j = i
            while j < n and src[j] in "<>&|=":
                j += 1
            i = j
            while i < n and src[i] in " \t":
                i += 1
            while i < n and src[i] not in " \t\n;&|<>()":
                i += 1
            continue
        op = next((o for o in OPERATORS if src.startswith(o, i)), None)
        if op:
            if op == "&" and word is not None and word.endswith((">", "<")):
                word += c
                i += 1
                continue
            flush()
            tokens.append(("op", op))
            i += len(op)
            continue
        word = (word or "") + c
        i += 1
    flush()
    return tokens


class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0

    def peek(self):
        return self.tokens[self.pos] if self.pos < len(self.tokens) else None

    def take(self):
        tok = self.peek()
        self.pos += 1
        return tok

    def parse_list(self, closer=None):
        items = []
        sep = None
        while True:
            tok = self.peek()
            if tok is None:
                break
            if tok == ("op", "\n") or tok == ("op", ";"):
                self.take()
                sep = sep or tok[1]
                continue
            if closer and tok in (("op", ")"), ("w", "}")) and tok[1] == closer:
                break
            items.append((sep, self.parse_and_or(closer)))
            tok = self.peek()
            sep = None
            if tok in (("op", ";"), ("op", "\n"), ("op", "&")):
                self.take()
                sep = tok[1]
        return items

    def parse_and_or(self, closer):
        chain = [(None, self.parse_pipeline(closer))]
        while self.peek() in (("op", "&&"), ("op", "||")):
            op = self.take()[1]
            while self.peek() == ("op", "\n"):
                self.take()
            chain.append((op, self.parse_pipeline(closer)))
        return chain

    def parse_pipeline(self, closer):
        cmds = [self.parse_command(closer)]
        while self.peek() in (("op", "|"), ("op", "|&")):
            self.take()
            while self.peek() == ("op", "\n"):
                self.take()
            cmds.append(self.parse_command(closer))
        return cmds

    def parse_command(self, closer):
        tok = self.peek()
        if tok == ("w", "{") or tok == ("op", "("):
            self.take()
            close = "}" if tok[1] == "{" else ")"
            body = self.parse_list(close)
            end = self.take()
            if end is None or end[1] != close:
                raise Unparsed
            return ("group", body)
        if tok and tok[0] == "w" and tok[1] in COMPOUND:
            return self.parse_compound()
        words = []
        while True:
            tok = self.peek()
            if tok is None or tok[0] == "op":
                break
            if closer == "}" and tok == ("w", "}"):
                break
            words.append(self.take()[1])
        if not words or words[0] in KEYWORDS:
            raise Unparsed
        return ("simple", words)

    def parse_compound(self):
        depth = 0
        at_start = True
        while True:
            tok = self.take()
            if tok is None:
                raise Unparsed
            if tok[0] == "op":
                at_start = tok[1] in (";", "\n", "&&", "||", "|", "&", "(", ";;")
                continue
            if at_start and tok[1] in COMPOUND:
                depth += 1
            elif at_start and tok[1] in COMPOUND_END:
                depth -= 1
                if depth == 0:
                    return ("compound", [])
            at_start = tok[1] in KEYWORDS


def git_write(words):
    i = 0
    while i < len(words) and "=" in words[i] and not words[i].startswith("="):
        i += 1
    while i < len(words) and words[i] in ("command", "env", "time", "nice", "nohup"):
        i += 1
    if i >= len(words) or os.path.basename(words[i]) != "git":
        return None
    i += 1
    while i < len(words) and words[i].startswith("-"):
        if words[i] in ("-C", "-c", "--git-dir", "--work-tree", "--namespace"):
            i += 1
        i += 1
    if i >= len(words):
        return None
    sub, rest = words[i], words[i + 1 :]
    if sub in ("commit", "push"):
        return f"git {sub}"
    if sub in CONTINUE_SUBCOMMANDS and "--continue" in rest:
        return f"git {sub} --continue"
    return None


def flags(items, found):
    for _, chain in items:
        for _, pipeline in chain:
            for kind, body in pipeline:
                if kind == "group":
                    flags(body, found)
                elif kind == "simple" and body[0] == "set":
                    args = " ".join(body[1:])
                    if "pipefail" in args:
                        found.add("pipefail")
                    if "errexit" in args or any(a.startswith("-") and "e" in a[1:] and not a.startswith("--") for a in body[1:]):
                        found.add("errexit")


def is_set_only(chain):
    return len(chain) == 1 and len(chain[0][1]) == 1 and chain[0][1][0] == ("simple", chain[0][1][0][1]) and chain[0][1][0][1][0] == "set"


def check(items, start_ok, opts, problems):
    reason = "inside a group that runs after an ungated step"
    ok = start_ok
    for index, (sep, chain) in enumerate(items):
        previous_is_set = index > 0 and is_set_only(items[index - 1][1])
        if index == 0:
            ok = start_ok
        elif sep in (";", "\n") and ("errexit" in opts or previous_is_set):
            ok = ok if previous_is_set else True
        else:
            ok = False
            reason = {";": "after `;`", "\n": "on a new line", "&": "after `&`"}.get(sep, "after `;`")
        for op, pipeline in chain:
            if op == "||":
                ok = False
                reason = "after `||`"
            for position, (kind, body) in enumerate(pipeline):
                if kind == "compound":
                    continue
                if kind == "group":
                    check(body, ok and position == 0, opts, problems)
                    continue
                write = git_write(body)
                if write and not ok:
                    problems.append((write, reason))
            if len(pipeline) > 1 and "pipefail" not in opts:
                if ok:
                    reason = "after a pipeline, whose exit status is only its last command's"
                ok = False
    return problems


def main():
    raw = sys.stdin.read() or os.environ.get("HOOK_INPUT", "")
    try:
        command = json.loads(raw)["tool_input"]["command"]
    except (ValueError, KeyError, TypeError):
        return 0
    if "git" not in command:
        return 0
    try:
        items = Parser(tokenize(command)).parse_list()
    except (Unparsed, IndexError, RecursionError):
        return 0
    opts = set()
    flags(items, opts)
    problems = check(items, True, opts, [])
    if not problems:
        return 0
    write, reason = problems[0]
    print(
        f"Refused: `{write}` runs {reason}, so it would run even if the step before it failed. "
        "Gate it on the exit status of the real check with `&&` (for example `cargo test && git commit …`), "
        "never on a grep of its output, and join every later step with `&&` too. "
        "Or run the git write as its own command after checking the result.",
        file=sys.stderr,
    )
    return 2


if __name__ == "__main__":
    sys.exit(main())
