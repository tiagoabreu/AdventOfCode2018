module Day7

open CommonLib.Files
open CommonLib.Lists

type Step = {
    Id: char
    mutable Requirements: list<char>
    }

let getOrderedSteps stepArr =
    Array.sortBy (fun x -> x.Id) stepArr

let rec getNextStep (currentStepList:list<char>) (stepArr:Step[]) =
    let isStepReady step =
        not (List.contains step.Id currentStepList) &&
        List.forall (fun r -> List.contains r currentStepList) step.Requirements
    let nextStep = Array.tryFind isStepReady stepArr

    if (nextStep.IsNone) then
        System.String.Concat(Array.ofList(currentStepList))
    else
        getNextStep (currentStepList @ [nextStep.Value.Id]) stepArr

let getExecutionOfStepsFromFile fileName =
    let mutable stepList = List.empty<Step>
    let parseLineIntoStep (line:string) =
        // format: Step V must be finished before step H can begin.
        let requirement = line.[5]
        let stepId = line.[36]
        if (Option.isNone (List.tryFind (fun r -> r.Id = requirement) stepList)) then 
            stepList <- stepList @ [{ Id = requirement; Requirements = List.empty<char> }]

        if (Option.isNone (List.tryFind (fun r -> r.Id = stepId) stepList)) then
            stepList <- stepList @ [{ Id = stepId; Requirements = [requirement] }]
        else
           let idx = List.findIndex (fun x -> x.Id = stepId) stepList
           let value = stepList.[idx]
           stepList <- removeAt idx stepList
           value.Requirements <- value.Requirements @ [requirement]
           stepList <- stepList @ [value]
    
    getLineValuesFromFilePath fileName
    |> Array.iter parseLineIntoStep

    stepList
    |> Array.ofList
    |> getOrderedSteps
    |> getNextStep list.Empty