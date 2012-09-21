require 'pathname'
require 'find'
require 'fileutils'
require 'erb'
require 'yaml'

task :default => [:update]

class TemplateRenderer
  def initialize(template)
    @template = ERB.new template
  end

  def get_binding
    binding
  end

  def secret(key)
    secrets_file = root_dir.join('secrets.yml')
    return '' unless File.exists?(secrets_file)
    @secrets ||= YAML.load_file(secrets_file)
    @secrets[key]
  end

  def result
    @template.result(get_binding)
  end
end

def debug?
  ENV['DEBUG']
end

def debug_output(string)
  puts string if debug?
end

def root_dir
  Pathname.new(File.dirname(__FILE__))
end

def home_dir
  Pathname.new(ENV['HOME'])
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
  if File.exists?(target)
    debug_output "    Target exists, removing.."
    File.unlink target
  end
end

def update_package(package)
  package_root = root_dir.join(package)
  debug_output "Updating #{package}.."
  Find.find(package_root) do |path|
    path = Pathname.new(path)
    unless path == package_root
      filename = path.relative_path_from(package_root).to_s

      # Path
      if File.directory?(path)
        FileUtils.mkdir_p target_for(filename)
        debug_output "- Creating directory #{target_for(filename)}"

      # Symlinks
      elsif filename =~ /\.symlink$/
        filename.gsub!(/\.symlink$/, '')
        debug_output "- Symlinking #{target_for(filename)}"
        clean_target(target_for(filename))
        FileUtils.ln_s path, target_for(filename)

      # ERB templates
      elsif filename =~ /\.erb$/
        filename.gsub!(/\.erb$/, '')
        debug_output "- Generating #{target_for(filename)}"
        clean_target(target_for(filename))
        template = TemplateRenderer.new File.read(path)
        File.open(target_for(filename), 'w') do |fh|
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

namespace :janus do
  desc "Install Janus"
  task :install do
    unless File.exists?(home_dir.join('.vim'))
      debug_output "Installing Janus"
      `curl -Lo- https://bit.ly/janus-bootstrap | bash`
    end
  end

  desc "Update Janus"
  task :update do
    if File.exists?(home_dir.join('.vim'))
      `cd "#{home_dir.join('.vim')}" && rake`
    else
      puts "Janus not found, run rake janus:install."
    end
  end
end

namespace :update do
  desc "Updates packages"
  task :packages do
    packages.each do |package|
      update_package(package)
    end
  end
end

desc "Updates everything"
task :update => [
  'janus:install',
  'update:packages'
] do
  puts "dotfiles updated"
end