# jev_endpoint prints without secrets

    Code
      print(jev_endpoint("https://gw.example.com/typesafe", "MY_GATEWAY_KEY", "m",
        name = "gw"))
    Output
      <jev_endpoint> gw
      URL:     https://gw.example.com/typesafe/v1/systemone
      API key: $MY_GATEWAY_KEY
      Model:   m

