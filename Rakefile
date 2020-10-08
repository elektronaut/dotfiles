require "pathname"
require "find"
require "fileutils"
require "erb"
require "tempfile"
require "yaml"

class TemplateRenderer
  def initialize(template)
    @template = ERB.new template
  end

  def get_binding
    binding
  end

  def secret(key)
    secrets_file = root_dir.join("secrets.yml")
    return "" unless File.exist?(secrets_file)
    @secrets ||= YAML.load_file(secrets_file)
    @secrets[key]
  end

  def osx?
    RUBY_PLATFORM.downcase.include?("darwin")
  end

  def linux?
    RUBY_PLATFORM.downcase.include?("linux")
  end

  def rbenv?
    ENV["RBENV"] || `which rbenv` =~ /(bin|libexec)\/rbenv/
  end

  def pow?
    File.exists?(home_dir.join(".pow"))
  end

  def result
    @template.result(get_binding)
  end
end

def debug?
  ENV["DEBUG"]
end

def debug_output(string)
  puts string if debug?
end

def root_dir
  Pathname.new(File.dirname(__FILE__))
end

def home_dir
  Pathname.new(ENV["HOME"])
end

def target_for(relative_path)
  home_dir.join(relative_path)
end

def packages
  @packages ||= Dir.entries(root_dir).select do |f|
    path = root_dir.join(f)
    !(f =~ /^\./) && File.directory?(path)
  end
end

def clean_target(target)
  return unless File.exist?(target)
  debug_output "    Target exists, removing.."
  FileUtils.rm_rf target
end

def update_package(package)
  package_root = root_dir.join(package)
  debug_output "Updating #{package}.."
  Find.find(package_root.to_s) do |path|
    path = Pathname.new(path)
    unless path == package_root
      filename = path.relative_path_from(package_root).to_s

      # Skip files under a symlinked dir
      if filename =~ /\.symlink\//
        # Do nothing

      # Symlinks
      elsif filename =~ /\.symlink$/
        filename.gsub!(/\.symlink$/, "")
        debug_output "- Symlinking #{target_for(filename)}"
        clean_target(target_for(filename))
        FileUtils.ln_s path, target_for(filename)

      # Path
      elsif File.directory?(path)
        FileUtils.mkdir_p target_for(filename)
        debug_output "- Creating directory #{target_for(filename)}"

      # ERB templates
      elsif filename =~ /\.erb$/
        filename.gsub!(/\.erb$/, "")
        debug_output "- Generating #{target_for(filename)}"
        clean_target(target_for(filename))
        template = TemplateRenderer.new File.read(path)
        File.open(target_for(filename), "w") do |fh|
          fh.write template.result
        end

      # Copying
      else
        debug_output "- Copying #{target_for(filename)}"
        clean_target(target_for(filename))
        FileUtils.cp path, target_for(filename)
      end
    end
  end
end

namespace :oh_my_zsh do
  desc "Install oh-my-zsh"
  task :install do
    unless File.exist?(home_dir.join(".oh-my-zsh"))
      debug_output "Installing oh-my-zsh"
      `wget --no-check-certificate https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh -O - | sh`
    end
  end

  desc "Update oh-my-zsh"
  task :update do
    if File.exist?(home_dir.join(".oh-my-zsh"))
      `cd "#{home_dir.join(".oh-my-zsh")}" && git checkout custom && git pull -q origin master`
    else
      puts "oh-my-zsh not found, run rake oh_my_zsh:install."
    end
  end

  desc "Delete oh-my-zsh"
  task :destroy do
    if File.exist?(home_dir.join(".oh-my-zsh"))
      FileUtils.rm_rf(home_dir.join(".oh-my-zsh"))
    end
  end
end

namespace :update do
  desc "Update crontab"
  task :crontab do
    crontab = `crontab -l 2> /dev/null`.strip
    unless crontab =~ /update_dotfiles/
      crontab += "\n03 * * * * ~/.bin/update_dotfiles >/dev/null 2> /dev/null"
      file = Tempfile.new("updated-cron")
      file.write(crontab + "\n")
      file.rewind
      file.close
      `crontab #{file.path}`
    end
  end

  desc "Updates packages"
  task :packages do
    packages.each do |package|
      update_package(package)
    end
  end

  desc "Updates everything"
  task :all => [
    "oh_my_zsh:update",
    "update"
  ] do
  end
end

task :default => [:update]

desc "Updates everything"
task :update => [
  "oh_my_zsh:update",
  "update:packages",
  "update:crontab"
] do
  puts "OK"
end

desc "Install"
task :install => [
  "oh_my_zsh:destroy",
  "oh_my_zsh:install",
  "update"
]
