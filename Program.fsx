namespace Stackcalculator

open System 

type Stack = float list 

module Parsing =

   //superfluous
   type Commands = string list 

   type Operation =
      | Push of float list
      | Pop 
      | Add 
      | Sub
      | Mul
      | Div 
      | Print
     

   // add more commands if needed 
   let commands : Commands = ["push"; "pop"; "add"; "sub"; "mul"; "div"; "print"] 


   let readline : Operation = 
      let input = Console.In.ReadLine().Split[|' '|] |> List.ofArray in //push will also have an argument
         match List.tryFind ((=) input.[0]) commands with 
         | None -> failwith ${input} is not recognized as a valid command" 
         | Some c -> match c with
                     | "push" -> Push (input.[1..] |> List.map float)
                     | "pop" -> Pop
                     | "add" -> Add
                     | "sub" -> Sub
                     | "mul" -> Mul
                     | "div" -> Div 
                     | "print" -> Print 



module Evaluation = 
   open Parsing  
      
   let empty () : Stack = []

   let push (stack : Stack) = (fun n -> n::Stack)
   
   let pop () = function | [] -> failwith "stack is empty" 
                         | stack -> List.tail stack) 

   let add = function | [] -> failwith "stack is empty" 
                      | [s] -> [s]
                      | x::y::stack -> (x+y)::stack 
      
   let sub = function | [] -> failwith "stack is empty" 
                      | [s] -> [s]
                      | x::y::stack -> (x-y)::stack   

   let mul = function | [] -> failwith "stack is empty" 
                      | [s] -> [s]
                      | x::y::stack -> (x*y)::stack   
  
   let mul = let tryDivide x y = try Some (x / y) with | :? System.DivideByZeroException -> None 
             function | [] -> failwith "stack is empty" 
                      | [s] -> [s]
                      | x::y::stack -> match tryDivide x y with 
                                       | Some n -> (x/y)::stack
                                       | None -> failwith "can't divide by zero" 

   let print = (fun stack -> List.iter (fun n -> printfn "%f" n)) 

 
open Evaluation  



