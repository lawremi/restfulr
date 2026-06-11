test_HTTP_user_agent_field <- function() {
  proto <- restfulr:::HTTP(user.agent = "MyApp/1.0")
  checkIdentical(proto$user.agent, "MyApp/1.0")
}

test_HTTP_user_agent_default_null <- function() {
  proto <- restfulr:::HTTP()
  checkTrue(is.null(proto$user.agent))
}

test_RestUri_user_agent_passthrough <- function() {
  uri <- RestUri("http://example.com", user.agent = "MyApp/1.0")
  checkIdentical(uri@protocol$user.agent, "MyApp/1.0")
}
