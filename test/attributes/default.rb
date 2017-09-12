default['authorization']['sudo']['include_sudoers_d'] = true

default['debian']['components'] = ["main", "contrib", "non-free"]

default['xvfb']['display'] = ':100'
default['xvfb']['screennum'] = '1'
default['xvfb']['dimensions'] = '1280x1024x24'
default['xvfb']['args'] = '-ac'

default['restaurant-test']['user'] = 'restaurant'
default['restaurant-test']['user_home'] = '/home/restaurant'

if node[:platform_family] == 'fedora' && node[:platform_version].to_i > 25
  default['restaurant-test']['ruby'] = '2.4' ## 2.3 is not building on new fedora
else
  default['restaurant-test']['ruby'] = '2.3'
end

default['restaurant-test']['gemset'] = 'global'

default['restaurant-test']['restaurant_directory'] = "#{node['restaurant-test']['user_home']}/restaurant"

