parse = Meta.parse

if isfile("lj-path")
    lj_path = readstring("lj-path")
    println("*** " * string(isfile(lj_path)))
else
    println("Please, specify path to LambdaJulia (lj.jl) in `lj-path` file in random-julia's root directory")
    exit(1)
end

include(lj_path)

blacklisting = false
if isfile("blacklist.jl")
    blacklisting = true
    include("blacklist.jl")
end

function lj_sub(t1 :: String, t2 :: String)
    blacklisting && 
        !is_notfound_marker(
            findfirst(equalto((t1,t2)), blacklist)) ||
        lj_subtype(t1, t2).sub
end

function two_copies(tstr :: String)
    tp = parse(tstr)
    (eval(tp), eval(tp))
end

proc(ts :: Vector) =
    for i in 1:length(ts)
        tistr = "$(ts[i])"
        #println("Type:\n\t" * tistr)
        (ti1, ti2) = two_copies(tistr)
        if !(ti1 <: ti2)
            println("Julia not reflexive on: ", tstr)
        end
        if !(ti1 <: ti2)
            println("Julia not reflexive on: ", tstr)
        end
        #println("Refl succeeded!!!")
        for j in i+1:length(ts)
            #println("Comparing\n\t $(ts[i])\nto\n\t$(ts[j])")
            res_jl = ts[i] <: ts[j]
            res_lj = lj_sub(tistr, "$(ts[j])")
            if res_lj != res_jl
                println("Diverge:\n\t", ts[i], "\n\t", ts[j], 
                        "\nJulia says $(res_jl) but LabdaJulia says $(res_lj).")
            end
        end
    end

function main()
    i = 1
    while true
        s = readline()
        if contains(s,"bye")
            break
        end
        println("Exp #$(i)")
        i += 1
        #println(s); println()
        ps = parse(s)
        #println("**********  PARSED  *********"); println()
        ts = eval(ps)
        #println(ts) ; println()
        #println("----------  EVALed  ----------\n")
        proc(ts)
    end
end
