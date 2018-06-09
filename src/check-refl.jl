function check(f :: String)
    for t in readlines(f)
          try
              p = parse(t) 
              tt = eval(p); ts = eval(p)
              if !(tt <: ts)
                  println("Counter example: $(t)")
              end
          catch e
            println("except on ", t)
          end
    end
end

check(ARGS[1])

