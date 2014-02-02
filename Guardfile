guard :shell do
  watch(%r{src/.+\.erl}) {|m| `erlc -I deps/ -I include/ #{m[0]} && mv #{File.basename(m[0], '.erl')}.beam ebin/` }
end

guard 'eunit' do
  watch(%r{test/.+\.erl})
  watch(%r{src/.+\.erl})
end
