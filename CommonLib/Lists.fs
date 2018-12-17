namespace CommonLib

module Lists = 
    let removeAt index input =
      input 
      |> List.mapi (fun i el -> (i <> index, el)) 
      |> List.filter fst |> List.map snd