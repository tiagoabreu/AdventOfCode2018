module Day8

open CommonLib.Files

type Node = {
    Metadata : int[]
    Childs : Node[] option
}

let rec parseIntoTree (data:int[]) =
    let numChilds = data.[0]
    let mutable childData = data.[2 .. data.Length - 1]
    let childArray = Array.init numChilds (fun _ -> {Metadata = Array.empty; Childs = None})
    for i = 0 to (numChilds-1) do
        if (childData.[0] = 0) then
            {Metadata = childData.[2 .. 1 + childData.[1]]; Childs = None}
            |> Array.set childArray i
            childData <- childData.[2 + childData.[1]..childData.Length - 1]
        else
            let result = parseIntoTree childData
            fst result
            |> Array.set childArray i
            childData <- snd result
    let metaData = childData.[0 ..data.[1]-1]
    childData <- childData.[data.[1] ..childData.Length - 1]
    { Metadata = metaData; Childs = Some childArray }, childData

let getTreeFromStr (str:string) =
    str.Split [|' '|]
    |> Array.map int
    |> parseIntoTree
    |> fst

let rec getSumOfMetadata node =
    let nodeSum = Array.sum node.Metadata
    match node.Childs with
    | Some childs -> nodeSum + (Array.sum <| Array.map getSumOfMetadata childs)
    | None -> nodeSum

let rec getRootNodeValue node =
    match node.Childs with
    | Some childs -> node.Metadata
                     |> Array.filter (fun i -> i <= childs.Length)
                     |> Array.map (fun x -> getRootNodeValue childs.[x-1])
                     |> Array.sum
    | None -> Array.sum node.Metadata

let getSumOfMetadataFromFileName fileName = 
    getLineValuesFromFilePath fileName
    |> Array.exactlyOne
    |> getTreeFromStr
    |> getSumOfMetadata

let getRootNodeValueFromFileName fileName = 
    getLineValuesFromFilePath fileName
    |> Array.exactlyOne
    |> getTreeFromStr
    |> getRootNodeValue