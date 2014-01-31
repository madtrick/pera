guard :shell do
  watch(%r{src/.+\.erl}) {|m| `erlc -I deps/ #{m[0]} && mv #{File.basename(m[0], '.erl')}.beam ebin/` }
end
