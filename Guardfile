guard :shell do
  watch(%r{src/.+\.erl}) do |m|
    if system "erlc +debug_info -I deps/ -I include/ #{m[0]}"
      `mv #{File.basename(m[0], '.erl')}.beam ebin/`  # Move the file to ebin so it can be reloaded
      #`dialyzer ebin/pera_*`                          # Dialize it
    end

  end
end

guard 'eunit' do
  watch(%r{test/.+\.erl})
  watch(%r{src/.+\.erl})
end
