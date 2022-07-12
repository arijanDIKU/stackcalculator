open System 


//superfluous
type Commands = string list 


type Stack = float list 

type Operation =
   | Push of float list
   | Pop of unit
   | Add 
   | Sub
   | Mul
   | Div 
   | Print
   | Quit of unit


// add more commands if needed 
let commands : string list = ["push"; "pop"; "add"; "sub"; "mul"; "div"; "print"; "quit"]


let readline (stream:System.IO.StreamReader) : Operation = 
   let input = let x = stream.ReadLine() in (x.Split[|' '|] |> List.ofArray) //push will also have an argument
   match (List.tryFind ((=) input.[0]) commands) with 
    | None -> failwith $"{input} is not recognized as a valid command" 
    | Some c -> match c with
                 | "push" -> if List.length input = 1 then failwith "please provide some input" 
                             else Push (input.[1..] |> List.map float)
                 | "pop" -> Pop ()
                 | "add" -> Add
                 | "sub" -> Sub
                 | "mul" -> Mul
                 | "div" -> Div 
                 | "print" -> Print 
                 | "quit" -> Quit ()



let empty () : Stack = []

let push (stack : Stack) = (function | [] -> stack | xs -> xs@stack) 

let pop () = function | [] -> failwith "stack is empty" 
                      | stack -> List.tail stack

let add = function | [] -> failwith "stack is empty" 
                   | [s] -> [s]
                   | x::y::stack -> (x+y)::stack 

let sub = function | [] -> failwith "stack is empty" 
                   | [s] -> [s]
                   | x::y::stack -> (x-y)::stack   

let mul = function | [] -> failwith "stack is empty" 
                   | [s] -> [s]
                   | x::y::stack -> (x*y)::stack   

let div = let tryDivide x y = try Some (x / y) with | :? System.DivideByZeroException -> None 
          function | [] -> failwith "stack is empty" 
                   | [s] -> [s]
                   | x::y::stack -> match tryDivide x y with 
                                     | Some n -> (x/y)::stack
                                     | None -> failwith "can't divide by zero" 

let print = (fun (stack:Stack) -> List.iter (printfn "%.1f") stack) 

let quit () = (fun stack -> print stack; printfn "Quitting...") 

let stream = new System.IO.StreamReader (System.Console.OpenStandardInput ())

let rec repl () (stack:Stack) : unit =
   printf "\n> " 
   match readline s with 
   | Push xs -> push stack xs |> repl () 
   | Pop p -> pop () stack |> repl ()
   | Add -> add stack |> repl ()
   | Sub -> sub stack |> repl () 
   | Mul -> mul stack |> repl ()
   | Div -> div stack |> repl ()
   | Print -> print stack; repl () stack
   | Quit q -> quit () stack



let stack = empty () 
printfn "fsharp stackcalculator version 0.1"

repl () stack 
