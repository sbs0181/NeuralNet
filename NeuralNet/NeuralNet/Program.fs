// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let Neuron (inputweights:float []) (inputvals:float []) (phi:float->float) = 
    inputvals |> Array.fold2 (fun acc elem1 elem2 -> acc+elem1*elem2) 0.0 inputweights |> phi


[<EntryPoint>]
let main argv = 
    let rate = 0.1
    let phi x = 1.0/(1.0+exp(-x))
    let weights = [| 0.1 ; 0.1 ; 0.1 ; 0.1 ; 0.1|]
    let inputs = [| 0.1 ; 1.0 ; 0.2 ; 0.3 ; 0.4|]
    let desiredout = 1.0
    
    let output = Neuron weights inputs phi

    output |> printf "%f\n"

    let dweights (outp:float) (doutp:float) (inp:float) = inp*(outp-doutp)*outp*(1.0-outp)
    
    dweights output desiredout inputs.[0] |> printf "%f\n"

    let newweights = inputs |> Array.map (dweights output desiredout)
                            |> Array.map2 (fun w dw -> w-rate*dw) weights
     
    Neuron newweights inputs phi |> printf "%f\n"

    weights |> Array.iter (printf "%f\t")
    printf "\n"
    newweights |> Array.iter (printf "%f\t")
    System.Console.ReadKey()|>ignore
    0 // return an integer exit code
