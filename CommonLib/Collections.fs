namespace CommonLib

module Collections =
    type Leaf = {value:int; left:Leaf option; right:Leaf option}

    let rec insertLeaf (tree:Leaf option) (newValue:int) =
        match tree with 
        | Some t -> if newValue < t.value then {t with left=Some (insertLeaf t.left newValue)}
                    else if newValue > t.value then {t with right=Some (insertLeaf t.right newValue)}
                    else t
        | None -> {value=newValue; left=None; right=None}

    let rec searchForLeaf (tree:Leaf option) (searchFor:int) : Leaf option = 
        match tree with 
        | Some t -> if searchFor = t.value then Some t
                    else if searchFor < t.value then searchForLeaf t.left searchFor
                    else if searchFor > t.value then searchForLeaf t.right searchFor
                    else None
        | None -> None

// Credit to https://lukemerrett.com/fsharp-binary-search-tree/