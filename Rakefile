task :test do
  sh "rebar eunit skip_deps=true"
end

task :shell do
  sh "erl -pa ebin deps/*/ebin"
end

task :start do
  #
  # Include `pwd` in the path to ebin so code:priv_dir works. See:
  #
  # http://erlang.org/pipermail/erlang-questions/2011-October/062024.html
  #
  sh "exec erl -pa `pwd`/ebin deps/*/ebin -boot start_sasl -s reloader -s pera"
end
