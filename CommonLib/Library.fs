namespace CommonLib

module Files =
    open System.IO

    let explode (s:string) =
            [|for c in s -> c|]

    let readLines (filePath:string) = seq {
            use sr = new StreamReader (filePath)
            while not sr.EndOfStream do
                yield sr.ReadLine ()
            }

    let getLineValuesFromFilePath filePath =
        [| for i in readLines filePath do
                yield i |]
