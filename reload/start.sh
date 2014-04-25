rebar clean compile
erl -pa ebin -code_path src/ -run reload_app start -heart
