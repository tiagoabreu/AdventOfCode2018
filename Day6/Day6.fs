module Day6

open CommonLib.Files

type Coordinate = {
    Id: int;
    X: int;
    Y: int
    }

let getMinX arrCoordinates =
    Array.map (fun x -> x.X) arrCoordinates
    |> Array.min

let getMaxX arrCoordinates =
    Array.map (fun x -> x.X) arrCoordinates
    |> Array.max

let getMinY arrCoordinates =
    Array.map (fun x -> x.Y) arrCoordinates
    |> Array.min

let getMaxY arrCoordinates =
    Array.map (fun x -> x.Y) arrCoordinates
    |> Array.max

let getDistanceMap x y arrCoordinates =
    Array.map (fun i -> i.Id, abs (i.X - x) + abs (i.Y - y)) arrCoordinates

let findNearestCoordinate x y arrCoordinates : (int) =
    let distanceMap = getDistanceMap x y arrCoordinates
    let minimum = Array.minBy (fun i -> snd i) distanceMap
    if (1 = (Array.filter (fun x -> snd x = snd minimum) distanceMap).Length) then
        fst minimum
    else
        0

let getSumOfDistances x y arrCoordinates =
    getDistanceMap x y arrCoordinates
    |> Array.sumBy (fun i -> snd i)

let getLargestFiniteArea arrCoordinates =
    let minX = getMinX arrCoordinates
    let maxX = getMaxX arrCoordinates
    let minY = getMinY arrCoordinates
    let maxY = getMaxY arrCoordinates

    let mutable edgeIds = List.Empty
    let mutable coordinatesIds = Map.ofArray <| Array.map (fun l -> l.Id, 0) arrCoordinates

    for x = minX to maxX do
        for y = minY to maxY do
            let coordinateId= findNearestCoordinate x y arrCoordinates
            if (coordinateId <> 0) then
                if (x = minX || x = maxX || y = minY || y = maxY) then 
                    edgeIds <- List.append edgeIds [coordinateId]
                coordinatesIds <- coordinatesIds.Add(coordinateId, coordinatesIds.[coordinateId] + 1)
    
    edgeIds <- List.distinct edgeIds
    Map.filter (fun k v-> not (List.contains (k) edgeIds)) coordinatesIds
    |> Map.toArray
    |> Array.maxBy (fun u -> snd u)
    |> snd
    
let getAreaWithLessThanSpecifiedDistanceSum target arrCoordinates =
    let minX = getMinX arrCoordinates
    let maxX = getMaxX arrCoordinates
    let minY = getMinY arrCoordinates
    let maxY = getMaxY arrCoordinates
    let mutable finalArea = 0
    for x = minX to maxX do
        for y = minY to maxY do
            if (target > getSumOfDistances x y arrCoordinates) then finalArea <- finalArea + 1
    finalArea

let parseCoordinate (str:int*string) =
    let x = (snd str).[0 .. (snd str).IndexOf(",") - 1] |> int
    let y = (snd str).[(snd str).IndexOf(" ") + 1 .. (snd str).Length - 1] |> int
    { Id = (fst str) + 1; X = x; Y = y}

let getLargestFiniteAreaFromFile fileName =
    getLineValuesFromFilePath fileName
    |> Array.indexed
    |> Array.map (parseCoordinate)
    |> getLargestFiniteArea

let getSafeAreaFromFile fileName =
    getLineValuesFromFilePath fileName
    |> Array.indexed
    |> Array.map (parseCoordinate)
    |> getAreaWithLessThanSpecifiedDistanceSum 10000