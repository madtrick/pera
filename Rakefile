task :test do
  sh "rebar eunit skip_deps=true"
end

task :shell do
  sh "erl -pa ebin deps/*/ebin"
end

task :start do
  sh "exec erl -pa ebin deps/*/ebin -boot start_sasl -s reloader -s pera"
end
