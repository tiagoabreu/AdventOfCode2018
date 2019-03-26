module Day11

let getValueOfCell x y serial =
    let rackId = (x + 1) + 10
    let hundredsDigit = abs ((serial + rackId * (y + 1)) * rackId / 100) % 10
    hundredsDigit - 5

let flat2DArray array2D = 
        [| for x in [0..(Array2D.length1 array2D) - 1] do 
                    for y in [0..(Array2D.length2 array2D) - 1] do 
                        yield array2D.[x, y] |]

let getLargestPowerLevelCoordinate serialNumber =
    let grid = Array2D.init 300 300 (fun x y -> getValueOfCell x y serialNumber)
    let rec calculatePowerLevelOfArea x y currentMaxAcc currentMaxSize targetX targetY currentAcc =

        if (targetX = 300 || targetY = 300) then
            if (y = 299) then printfn "%A %A" x y
            currentMaxAcc, currentMaxSize
        else
            let xSum = Array.sumBy (fun i -> if ( i < 300 && targetY < 300) then grid.[i,targetY] else 0) 
                        <| [|x..targetX|]
            let ySum = Array.sumBy (fun j -> if (targetX < 300 && j < 300) then grid.[targetX,j] else 0) 
                        <| [|y..targetY|]
            let acc =  currentAcc + xSum + ySum - grid.[targetX,targetY]

            if (acc > currentMaxAcc) then calculatePowerLevelOfArea x y acc (targetX-x+1) (targetX + 1) (targetY + 1) acc
            else calculatePowerLevelOfArea x y currentMaxAcc currentMaxSize (targetX + 1) (targetY + 1) acc

    let areaPowerLevelMap = Array2D.mapi (fun x y _ -> calculatePowerLevelOfArea x y 0 0 x y 0) grid

    Array2D.zeroCreate 300 300
    |> Array2D.mapi (fun i j _ -> areaPowerLevelMap.[i,j],i,j)
    |> flat2DArray
    |> Array.maxBy (fun (v,_,_) -> fst v)
    |> (fun (v,x,y) -> x+1, y+1,snd v)


