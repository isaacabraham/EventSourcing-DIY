#r "netstandard"
#load @"..\.paket\load\netstandard2.0\main.group.fsx"
      @"Step4\Infrastructure4.fs"
      @"Step4\Domain4.fs"
      @"Step4\Program4.fs"
      @"Step4\Tests4.fs"

open Step4.Domain
open Step4.Domain.Projections
open FsCheck
open FsCheck.Experimental

/// A simplified model that tracks the stock of all flavours.
type Model = Map<Flavour, int>

/// A wrapper around the real event system.
type RealSystem(initialStock) =
    /// Tracks the events in the system.
    let mutable events =
        initialStock
        |> Map.toSeq
        |> Seq.map(fun (flavour, stock) -> Flavour_restocked(flavour, stock))
        |> ResizeArray

    /// Restocks flavour by one.
    member __.Restock flavour =
        events |> Behaviour.restock flavour 1 |> events.AddRange
        
        events
        |> Seq.toList
        |> project flavoursInStock
        |> stockOf flavour

    /// Tries to sell a single flavour instance.
    member __.Sell flavour =
        events |> Seq.toList |> Behaviour.sellFlavour flavour |> events.AddRange

        events
        |> Seq.toList
        |> project flavoursInStock
        |> stockOf flavour

let spec =
    /// Restocks the flavour by one
    let restock flavour = 
        { new Operation<RealSystem, Model>() with
            member __.Run m =
                m |> Map.add flavour (m.[flavour] + 1)
            member __.Check (system, model) = 
                let res = system.Restock flavour
                model.[flavour] = res
                |@ sprintf "Restock %A: model = %i, actual = %i" flavour model.[flavour] res
            override __.ToString() = sprintf "restock %A" flavour }

    /// Tries to sell when stock is non-empty
    let sellWhenValid flavour = 
        { new Operation<RealSystem,Model>() with
            member __.Run m =
                m |> Map.add flavour (m.[flavour] - 1)
            override __.Pre m =
                m.[flavour] > 0
            member __.Check (system, m) = 
                let res = system.Sell flavour
                m.[flavour] = res
                |@ sprintf "Sell %A: model = %i, actual = %i" flavour m.[flavour] res
            override __.ToString() = sprintf "sell %A" flavour }

    /// Tries to sell when stock is empty
    let sellWheninvalid flavour = 
        { new Operation<RealSystem,Model>() with
            member __.Run m = m
            override __.Pre m =
                m.[flavour] = 0
            member __.Check (system, m) = 
                let res = system.Sell flavour
                m.[flavour] = res
                |@ sprintf "Cant Sell %A: model = %i, actual = %i" flavour m.[flavour] res
            override __.ToString() = sprintf "cantSell %A" flavour }

    /// Creates the initial stock of the system
    let create initialStock = 
        { new Setup<RealSystem,Model>() with
            member __.Actual() = RealSystem initialStock
            member __.Model() = initialStock }
    
    let allFlavours = [ Vanilla; Strawberry ]

    { new Machine<RealSystem,Model>() with
        member __.Setup =
            allFlavours
            |> List.map(fun flavour ->
                Gen.choose (0, 3)
                |> Gen.map(fun stock -> flavour, stock))
            |> Gen.collect id
            |> Gen.map(Map >> create)
            |> Arb.fromGen
        member __.Next _ =
            gen {
                let! operations =
                    allFlavours
                    |> List.map(fun f -> Gen.elements [ restock f; sellWhenValid f; sellWheninvalid f ])
                    |> Gen.collect id
                return! Gen.elements operations
            }
    }

let p = spec |> StateMachine.toProperty

Check.Quick p
// Check.Verbose p
