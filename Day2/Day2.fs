module Day2

open CommonLib.Files

let arrayHasElementRepeated arr numOfRepetitions =
    let getOccurencesPerLetter str =
        Array.countBy (fun elem -> elem) str

    let isCorrectNumberOfOccurences (occurenceElem : 'a * int) =
        snd occurenceElem = numOfRepetitions

    getOccurencesPerLetter arr
    |> Array.tryFind isCorrectNumberOfOccurences
    |> Option.isSome

let calculateCheckSum (arr:int[]) =
    Array.fold (fun acc cur -> acc * cur) 1 arr

let incrementCountInCheckSum (checkSumValues:(int * int)[]) valueToIncrement =
    let index = Array.findIndex (fun x -> fst x = valueToIncrement) checkSumValues
    let tupletoChange = checkSumValues.[index]
    Array.set checkSumValues index (valueToIncrement, snd tupletoChange + 1)

let getCheckSumOfIds (repetitionCheck:int[]) (idArr:string[]) =
    let checkSumValues : (int * int)[] = Array.map (fun x -> (x, 0)) repetitionCheck

    let processRepetitions str =
        let processRepetition charArr repetition  =
            if (arrayHasElementRepeated charArr repetition) then
                    incrementCountInCheckSum checkSumValues repetition

        let charArr = explode str
        Array.iter (processRepetition charArr) repetitionCheck

    Array.iter processRepetitions idArr

    Array.map (fun x -> snd x) checkSumValues
    |> calculateCheckSum

let getCheckSumOfFile fileName (repetitionCheck:int[]) =
    getLineValuesFromFilePath fileName
    |> getCheckSumOfIds repetitionCheck

let getValidId (str1:char[]) (str2:char[]) =
    let indexedArr = Array.indexed str1
    let diffChars = Array.filter (fun x -> str2.[fst x] <> snd x) indexedArr
    if (diffChars.Length = 1) then
        Array.filter (fun x -> fst x <> fst diffChars.[0]) indexedArr
        |> Array.map (fun y -> snd y)
        |> System.String
    else
        ""

let rec getValidIdInArray (idArray:char[][]) =
    if (idArray.Length <= 1) then
        ""
    else
        let head = Array.head idArray
        let tail = Array.tail idArray
        let result = Array.map (getValidId head) tail
                    |> Array.tryFind (fun x -> x.Length > 0)
        if (result.IsSome) then
            result.Value
        else 
            getValidIdInArray tail

let getValidIdInFile fileName = 
    getLineValuesFromFilePath fileName
    |> Array.map explode
    |> getValidIdInArray