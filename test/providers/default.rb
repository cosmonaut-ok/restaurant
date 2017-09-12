def whyrun_supported?
  true
end

use_inline_resources

action :create do
  run_context.include_recipe 'ohai'
  run_context.include_recipe 'debian' if node['platform_family'] == 'debian'
  run_context.include_recipe 'xvfb'

  cache_dir = Chef::Config[:file_cache_path]

  ## install common packages
  %w[sudo].each do |pkg|
    package pkg
  end

  ## install fedora-specific packages
  %w[ImageMagick ruby-devel redhat-lsb-core].each do |rpm|
    package rpm
  end if node['platform_family'] == 'fedora'

  ## install debian-specific packages
  %w[imagemagick ruby-dev].each do |dpkg|
    package dpkg
  end if node['platform_family'] == 'debian'

  user new_resource.user do
    home new_resource.user_home
    manage_home true
    shell '/bin/bash'
  end

  sudo new_resource.user do
    user new_resource.user
    nopasswd true
  end
  
  cookbook_file "#{cache_dir}/restaurant.tar.gz" do
    source 'restaurant.tar.gz'
    user new_resource.user
    notifies :extract_local, "tar_extract[#{cache_dir}/restaurant.tar.gz]", :immediately
  end

  tar_extract "#{cache_dir}/restaurant.tar.gz" do
    target_dir new_resource.user_home
    creates "#{new_resource.location}/bootstrap.sh"
    user new_resource.user
    action :nothing
  end
    
  directory "#{new_resource.location}/etc" do
    owner new_resource.user
    recursive true
  end

  template "#{new_resource.location}/etc/restaurant.conf" do
    cookbook 'restaurant-test'
    source 'restaurant.conf.erb'
    user new_resource.user
    variables(ruby_version: new_resource.ruby_version, ruby_gemset: new_resource.ruby_gemset)
    notifies :run, "bash[bootstrap.sh #{new_resource.ruby_version} #{new_resource.ruby_gemset}]", :immediately
  end
  
  bash "bootstrap.sh #{new_resource.ruby_version} #{new_resource.ruby_gemset}" do
    code <<-EOF
## We should emulate user environment
/bin/bash -l #{new_resource.location}/bootstrap.sh #{new_resource.ruby_version} #{new_resource.ruby_gemset}
  EOF
    creates "#{new_resource.user_home}/.rvm/rubies/default/bin/ruby"
    cwd new_resource.location
    user new_resource.user
    environment({
                  :HOME => new_resource.user_home,
                  :USER => new_resource.user
                })
    not_if { ::File.exists?("#{new_resource.user_home}/.rvm/rubies/default/bin/ruby") }
  end

  bash 'restaurant batch' do
    code "#{new_resource.location}/restaurant --batch"
    cwd new_resource.location
    user new_resource.user
    environment({
                  :HOME => new_resource.user_home,
                  :USER => new_resource.user
                })
    only_if { ::File.exists?("#{new_resource.user_home}/.rvm/rubies/default/bin/ruby") && ::File.exists?("#{new_resource.location}/restaurant") }
  end





#   run_context.include_recipe 'ark'
#   run_context.include_recipe 'tar'
#   run_context.include_recipe 'tc-java'

#   java_opts = new_resource.java_opts
#   environment_data = {}
  
#   ## use node['microservices']['env'] as global
#   if node['microservices'] && node['microservices'].has_key?('env') && node['microservices']['env'].respond_to?(:has_key?)
#     environment_data = node['microservices']['env']
#   end

#   ## merge with local environment data
#   if node[new_resource.name] && node[new_resource.name].has_key?('env')
#     environment_data = Chef::Mixin::DeepMerge.deep_merge(node[new_resource.name]['env'],
#                                                          environment_data)
#   end

#   ## get secret data bag data
#   secret_data = get_secret(new_resource.name, node.chef_environment)
#   if secret_data && secret_data.has_key?('env') && secret_data['env'].respond_to?(:has_key?)
#     environment_data = Chef::Mixin::DeepMerge.deep_merge(secret_data['env'],
#                                                          environment_data)
#   end

#   user new_resource.user do
#     home new_resource.install_dir
#     system true
#   end

#   case new_resource.maven_packaging
#   when 'jar'
#     install_fatjar
#   when 'tar.gz'
#     install_tgz
#   when 'tgz'
#     install_tgzdist
#   else
#     raise "Don't know how to handle a #{new_resource.maven_packaging} package"
#   end

#   # keystore management
#   if new_resource.use_additional_keystore
#     cookbook_file new_resource.additional_keystore_file do # ~FC021
#       source "#{new_resource.name}/#{node.chef_environment}-keystore.jks"
#       owner new_resource.user
#       group new_resource.user
#       action :create
#       only_if { new_resource.use_additional_keystore }
#       notifies :restart, "service[#{new_resource.service_name}]"
#     end

#     java_opts = "#{java_opts} -Djavax.net.ssl.trustStore=#{new_resource.additional_keystore_file}"
#     # check if keystore password is set
#     if new_resource.additional_keystore_password != ''
#       java_opts = "#{java_opts} -Djavax.net.ssl.trustStorePassword=#{new_resource.additional_keystore_password}"
#     end
#   end

#   # logs forwarder to remote logstash
#   if Chef::Config[:solo]
#     Chef::Log.warn('This recipe uses search. Chef Solo does not support search.')
#   else
#     logstash_servers = search(:node, "role:kibana AND chef_environment:#{node.chef_environment}").sort
#     node.default['microservices']['logstash']['hostname'] = logstash_servers.empty? ? node['microservices']['logstash']['hostname']: logstash_servers.first[:fqdn]

#     if new_resource.log_logstash && (!node[new_resource.name] || \
#                                      !node[new_resource.name]['env'] || \
#                                      (!node[new_resource.name]['logstash'] && \
#                                       !node['microservices']['logstash']
#                                      )
#                                     )
#       raise 'Logstash support enabled, but logstash unconfigured. Can not continue'
#     else
#       enable_logstash = new_resource.log_logstash
#     end

#     template "#{new_resource.install_dir}/conf/logback.xml" do
#       source new_resource.log_template
#       cookbook 'tc-java-service'
#       owner new_resource.user
#       group new_resource.user
#       variables ({
#                   app_dir: new_resource.install_dir,
#                   service_name: new_resource.service_name,
#                   service_version: new_resource.version,
#                   log_level: new_resource.log_level,
#                   logstash_appender: enable_logstash,
#                   logback_timesize_policy: new_resource.logback_timesize_policy,
#                   maxHistory: new_resource.maxHistory,
#                   maxFileSize: new_resource.maxFileSize,
#                   totalSizeCap: new_resource.totalSizeCap,
#                   logstash_service_instance_id: environment_data['SERVICE_INSTANCE_ID'] || "#{new_resource.name}-0001",
#                   logstash_service_name: environment_data['SERVICE_NAME'] || new_resource.name,
#                   logstash_host: node['microservices']['logstash']['hostname'] || node[new_resource.name]['logstash']['hostname'],
#                   logstash_port: node['microservices']['logstash']['port'] || node[new_resource.name]['logstash']['port'],
#                   logstash_loglevel: node['microservices']['logstash']['loglevel'] || node[new_resource.name]['logstash']['loglevel']
#                  })
#       notifies :restart, "service[#{new_resource.service_name}]"
#     end
#   end

#   local_log_path = "#{new_resource.install_dir}/logs"

#   tc_rsync_logs new_resource.service_name do
#     path local_log_path
#     schedule node['tc-rsync']['schedule']
#     action :enable
#   end

#   # HACK: logrotate_app is a definition not a LWRP, so we can't use
#   #       `current_resource.name` inside the logrotate_app block
#   log_user = new_resource.user
#   log_app = new_resource.service_name
#   logrotate_postrotate = <<-EOH
#     sleep 1; service #{new_resource.service_name} restart;
#   EOH

#   logrotate_app log_app do
#     cookbook 'logrotate'
#     path local_log_path
#     frequency 'daily'
#     create "644 #{log_user} #{log_user}"
#     rotate 10
#     maxsize 104_857_600
#     postrotate logrotate_postrotate
#   end

#   if node['newrelic']['enable_application_monitoring?']
#     newrelic_settings = node['newrelic'][node['newrelic']['area']]
#     newrelic_path = ::File.join(newrelic_settings['home'], new_resource.service_name)

#     tc_monitoring_java new_resource.service_name do
#       version newrelic_settings['version']
#       download_url newrelic_settings['download_url']
#       install_dir newrelic_path
#       owner new_resource.user
#       group new_resource.user
#       action :install
#       notifies :restart, "service[#{new_resource.service_name}]", :delayed
#     end

#     java_opts_full = "#{java_opts} -javaagent:#{newrelic_path}/newrelic/newrelic.jar  -Dlogback.configurationFile=file:#{new_resource.install_dir}/conf/logback.xml -Dlogging.config=file:#{new_resource.install_dir}/conf/logback.xml"
#     java_command_line = "#{new_resource.java_bin} #{java_opts_full} -jar #{new_resource.install_dir}/bin/#{new_resource.main_jar}.jar"

#   else
#     java_opts_full = "#{java_opts} -Dlogback.configurationFile=file:#{new_resource.install_dir}/conf/logback.xml -Dlogging.config=file:#{new_resource.install_dir}/conf/logback.xml"
#     java_command_line = "#{new_resource.java_bin} #{java_opts_full} -jar #{new_resource.install_dir}/bin/#{new_resource.main_jar}.jar"
#   end

#   user_properties = get_user_properties(new_resource.rabbitmq_login)
#   rabbitmq_pass = user_properties['password']
#   rabbitmq_vhost = user_properties['vhost']

#   template "#{new_resource.install_dir}/conf/env.sh" do
#     source new_resource.env_template
#     cookbook new_resource.env_template_cookbook
#     owner new_resource.user
#     group new_resource.user
#     mode '0644'
#     variables ({
#                 # install_dir: new_resource.install_dir,
#                 # service_name: new_resource.service_name,
#                 service_desc: new_resource.service_desc,
#                 msb_login: new_resource.rabbitmq_login,
#                 msb_pass: rabbitmq_pass,
#                 msb_vhost: new_resource.rabbitmq_vhost || rabbitmq_vhost,
#                 data: environment_data
#                })
#     notifies :restart, "service[#{new_resource.service_name}]"
#   end

#   template "#{new_resource.install_dir}/run.sh" do
#     cookbook 'tc-java-service'
#     source 'run.sh.erb'
#     owner new_resource.user
#     group new_resource.user
#     mode '0755'
#     variables ({
#                 install_dir: new_resource.install_dir,
#                 java_command_line: java_command_line,
#                })
#     notifies :restart, "service[#{new_resource.service_name}]"
#   end

#   template "/etc/init.d/#{new_resource.service_name}" do
#     cookbook 'tc-java-service'
#     source 'java-service.init.erb'
#     owner 'root'
#     group 'root'
#     mode '0755'
#     variables ({
#                 install_dir: new_resource.install_dir,
#                 service_name: new_resource.service_name,
#                 service_desc: new_resource.service_desc,
#                 run_login: new_resource.user,
#                })
#     notifies :restart, "service[#{new_resource.service_name}]"
#   end

#   service new_resource.service_name do
#     action new_resource.service_status
#     supports status: true, start: true, stop: true, restart: true
#   end
# end

# def install_fatjar
#   jarfile = "#{new_resource.install_dir}/bin/#{new_resource.main_jar}.jar"
#   versioned_jarfile = "#{new_resource.install_dir}/bin/#{new_resource.main_jar}-#{new_resource.version}.jar"

#   directory new_resource.install_dir do
#     owner new_resource.user
#     mode '0755'
#   end

#   directory "#{new_resource.install_dir}/logs" do
#     owner new_resource.user
#     mode '0755'
#   end

#   directory "#{new_resource.install_dir}/bin" do
#     owner new_resource.user
#     mode '0755'
#   end

#   directory "#{new_resource.install_dir}/conf" do
#     owner new_resource.user
#     mode '0755'
#   end

#   remote_file versioned_jarfile do
#     source new_resource.download_url
#     owner new_resource.user
#     mode '0755'
#   end

#   link jarfile do
#     owner new_resource.user
#     to versioned_jarfile
#     action :create
#     notifies :restart, "service[#{new_resource.service_name}]"
#   end
# end

# def install_tgz
#   ark new_resource.name do
#     url new_resource.download_url
#     version new_resource.version
#     checksum new_resource.checksum
#     home_dir new_resource.install_dir
#     owner new_resource.user
#     action :install
#     notifies :restart, "service[#{new_resource.service_name}]"
#   end
# end

# def install_tgzdist

#   jarfile = "#{new_resource.install_dir}/bin/#{new_resource.main_jar}.jar"
#   versioned_jarfile = "#{new_resource.install_dir}/bin/#{new_resource.main_jar}-#{new_resource.version}.jar"

#   directory new_resource.install_dir do
#     owner new_resource.user
#     mode '0755'
#   end

#   directory "#{new_resource.install_dir}/logs" do
#     owner new_resource.user
#     mode '0755'
#   end

#   directory "#{new_resource.install_dir}/bin" do
#     owner new_resource.user
#     mode '0755'
#   end

#   directory "#{new_resource.install_dir}/conf" do
#     owner new_resource.user
#     mode '0755'
#   end

#   tar_extract new_resource.download_url do
#     target_dir "#{new_resource.install_dir}/bin"
#     creates "#{new_resource.install_dir}/bin/#{new_resource.version}.jar"
#   end

#   link jarfile do
#     owner new_resource.user
#     to versioned_jarfile
#     action :create
#     notifies :restart, "service[#{new_resource.service_name}]"
#   end
end
