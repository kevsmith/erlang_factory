require 'json'
require 'rest_client'

module DemoClient
  class Resource < RestClient::Resource

    def get(additional_headers={})
      parse_result(super(additional_headers.merge({:accept => 'application/json', :content_type => 'application/json'})))
    end

    def put(payload, additional_headers={})
      parse_result(super(payload.to_json, additional_headers.merge({:accept => 'application/json', :content_type => 'application/json'})))
    end

    def delete(additional_headers={})
      parse_result(super(additional_headers.merge({:accept => 'application/json', :content_type => 'application/json'})))
    end

    def post()
      parse_result(super('', {:accept => 'application/json', :content_type => 'application/json'}))
    end

    def post(payload, additional_headers={})
      parse_result(super(payload.to_json, additional_headers.merge({:accept => 'application/json', :content_type => 'application/json'})))
    end

    private
    def parse_result(result)
      result = JSON.parse(result) unless result == nil
      result
    end

  end

end
