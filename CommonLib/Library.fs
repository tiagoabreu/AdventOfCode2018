namespace CommonLib

module Files =
    open System.IO

    let getValuesFromFilePath filePath =
        let explode (s:string) =
            [|for c in s -> c|]
        let readLines (filePath:string) = seq {
            use sr = new StreamReader (filePath)
            while not sr.EndOfStream do
                yield sr.ReadLine ()
            }

        let programArr = [|for i in readLines filePath do
                            yield explode i  |]

        programArr.[0]
