module Day7

open CommonLib.Files
open CommonLib.Lists

type Step = {
    Id: char
    mutable Requirements: list<char>
    Duration : int
    mutable RemainingDuration : int
    }

let getOrderedSteps stepArr =
    Array.sortBy (fun x -> x.Id) stepArr

let isStepReady currentStepList step =
        not (List.contains step.Id currentStepList) &&
        List.forall (fun r -> List.contains r currentStepList) step.Requirements

let rec getNextStep (currentStepList:list<char>) (stepArr:Step[]) =
    let nextStep = Array.tryFind (isStepReady currentStepList) stepArr

    if (nextStep.IsNone) then
        System.String.Concat(Array.ofList(currentStepList))
    else
        getNextStep (currentStepList @ [nextStep.Value.Id]) stepArr

let getStepDuration (c:char) =
    int c - int 'A' + 61

let getWorkingDurationOfStepArray numWorkers stepArr =
    let isStepReadyForPickup doneList (inProgress:option<Step>[]) step =
        isStepReady doneList step && Option.isNone <| Array.tryFind (fun x -> Option.isSome x && x.Value.Id = step.Id) inProgress
        
    let inProgress : option<Step>[] = Array.create numWorkers None
    let mutable secondsElapsed = 0
    let mutable doneSteps = list.Empty

    while (doneSteps.Length <> Array.length stepArr) do
        secondsElapsed <- secondsElapsed + 1
        for i,v in Array.indexed inProgress do
            if (Option.isSome v) then
                let updatedStep = v
                updatedStep.Value.RemainingDuration <- updatedStep.Value.RemainingDuration - 1
                Array.set inProgress i updatedStep
            else
                let nextStep = Array.tryFind (isStepReadyForPickup doneSteps inProgress) stepArr
                if (Option.isSome nextStep) then
                    nextStep.Value.RemainingDuration <- nextStep.Value.RemainingDuration - 1
                    Array.set inProgress i nextStep

        for i,v in Array.indexed inProgress do
            if (Option.isSome v && v.Value.RemainingDuration = 0) then
                doneSteps <- doneSteps @ [v.Value.Id]
                Array.set inProgress i None

    secondsElapsed

let getStepListFromFile fileName = 
    let mutable stepList = List.empty<Step>
    let parseLineIntoStep (line:string) =
        // format: Step V must be finished before step H can begin.
        let requirement = line.[5]
        let stepId = line.[36]
        if (Option.isNone (List.tryFind (fun r -> r.Id = requirement) stepList)) then 
            stepList <- stepList @ [{ Id = requirement; 
                                      Requirements = List.empty<char>; 
                                      Duration = getStepDuration requirement; 
                                      RemainingDuration = getStepDuration requirement}]

        if (Option.isNone (List.tryFind (fun r -> r.Id = stepId) stepList)) then
            stepList <- stepList @ [{ Id = stepId; 
                                      Requirements = [requirement]; 
                                      Duration = getStepDuration stepId; 
                                      RemainingDuration = getStepDuration stepId }]
        else
           let idx = List.findIndex (fun x -> x.Id = stepId) stepList
           let value = stepList.[idx]
           stepList <- removeAt idx stepList
           value.Requirements <- value.Requirements @ [requirement]
           stepList <- stepList @ [value]
    
    getLineValuesFromFilePath fileName
    |> Array.iter parseLineIntoStep

    stepList

let getExecutionOfStepsFromFile fileName =
    fileName
    |> getStepListFromFile 
    |> Array.ofList
    |> getOrderedSteps
    |> getNextStep list.Empty

let getExecutionTimeOfStepsFromFile fileName =
    fileName
    |> getStepListFromFile 
    |> Array.ofList
    |> getOrderedSteps
    |> getWorkingDurationOfStepArray 5