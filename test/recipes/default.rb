#
# Cookbook Name:: restaurant-test
# Recipe:: default
#
# Copyright (C) 2017 YOUR_NAME
#
# All rights reserved - Do Not Redistribute
#

include_recipe 'ohai'
include_recipe 'debian' if node['platform_family'] == 'debian'

include_recipe 'xvfb'

user_home = node['restaurant-test']['user_home']
cache_dir = Chef::Config[:file_cache_path]

%w[imagemagick sudo].each do |pkg|
   package pkg
end

## install debian-specific packages
%w[ruby-dev].each do |dpkg|
  package dpkg
end if node['platform_family'] == 'debian'

user node['restaurant-test']['user'] do
  home user_home
  manage_home true
  shell '/bin/bash'
end

sudo node['restaurant-test']['user'] do
  user node['restaurant-test']['user']
  nopasswd true
end

directory node['restaurant-test']['restaurant_directory'] do
  owner node['restaurant-test']['user']
  recursive true
end

cookbook_file "#{cache_dir}/restaurant.tar.gz" do
  source 'restaurant.tar.gz'
  user node['restaurant-test']['user']
  notifies :extract_local, "tar_extract[#{cache_dir}/restaurant.tar.gz]", :immediately
end

tar_extract "#{cache_dir}/restaurant.tar.gz" do
  target_dir user_home
  creates "#{node['restaurant-test']['restaurant_directory']}/bootstrap.sh"
  user node['restaurant-test']['user']
  action :nothing
  notifies :run, 'bash[bootstrap.sh]', :immediately
end

bash 'bootstrap.sh' do
  code <<-EOF
## We should emulate user environment
/bin/bash -l #{node['restaurant-test']['restaurant_directory']}/bootstrap.sh #{node['restaurant-test']['ruby']} #{node['restaurant-test']['gemset']}
  EOF
  creates "#{user_home}/.rvm/rubies/default/bin/ruby"
  cwd node['restaurant-test']['restaurant_directory']
  user node['restaurant-test']['user']
  environment({
                :HOME => user_home,
                :USER => node['restaurant-test']['user']
              })
  not_if { ::File.exists?("#{user_home}/.rvm/rubies/default/bin/ruby") }
end

bash 'restaurant batch' do
  code "#{node['restaurant-test']['restaurant_directory']}/restaurant --batch"
  cwd node['restaurant-test']['restaurant_directory']
  user node['restaurant-test']['user']
  environment({
                :HOME => user_home,
                :USER => node['restaurant-test']['user']
              })
  only_if { ::File.exists?("#{user_home}/.rvm/rubies/default/bin/ruby") && ::File.exists?("#{node['restaurant-test']['restaurant_directory']}/restaurant") }
end
