# https://docs.chef.io/release/11-18/custom_resources.html
# provides :restaurant_deployment

actions :create
default_action :create

attribute :name, kind_of: String, required: true, name_attribute: true
attribute :ruby_version, kind_of: String, required: false, default: lazy {
  if node[:platform_family] == 'fedora' && node[:platform_version].to_i > 25
    '2.4' ## 2.3 is not building on new fedora
  else
    '2.3'
  end
}

attribute :ruby_gemset, kind_of: String, required: false, default: 'global'
attribute :user, kind_of: String, required: false, default: lazy {|r| r.name}
attribute :user_home, kind_of: String, required: false, default: lazy {|r| "/home/#{r.user}"}
attribute :location, kind_of: String, required: false, default: lazy {|r| "#{r.user_home}/restaurant"}
attribute :use_external_emacs, kind_of: [TrueClass, FalseClass], default: false
