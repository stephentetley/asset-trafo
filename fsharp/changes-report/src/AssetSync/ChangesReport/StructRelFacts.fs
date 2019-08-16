// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangesReport


module StructRelFacts =

    open System.IO
    open FSharp.Data

    open FactX
    open FactX.FactWriter

    [<Literal>]
    let AideStructRelSchema = 
        "StructureRelationshipId(int64),\
         RuleId(int64),\
         ParentId(int64),\
         ChildId(int64)"    

    [<Literal>]
    let AideStructRelSample = 
        "1,4000,150,2500"
       

    type AideStructRelTable = 
        CsvProvider< Schema = AideStructRelSchema 
                   , Sample = AideStructRelSample
                   , HasHeaders = true >

    type AideStructRelRow = AideStructRelTable.Row

    let getAideStructRelRows (cvsPath : string) : seq<AideStructRelRow> = 
        let table = AideStructRelTable.Load(uri = cvsPath)
        table.Rows

    let makeAideStructRelFact (row : AideStructRelRow) : Predicate option = 
        predicate "aide_relationship" 
                    [ int64Term row.StructureRelationshipId
                    ; int64Term row.RuleId
                    ; int64Term row.ParentId
                    ; int64Term row.ChildId
                    ] |> Some

    let generatePredicates (makePred : 'row -> Predicate option) 
                            (source : 'row list) : FactWriter<unit> =
        List.choose makePred source
            |> List.distinct
            |> List.sort
            |> mapMz tellPredicate

    let writeFactsWithHeaderComment (outputFile : string)
                                      (factOutput: FactWriter<unit>) : unit =
          let shortName = Path.GetFileName outputFile
          let audit = sprintf "Generated: %s" (System.DateTime.Now.ToString(format = "yyyy-MM-dd HH:mm:ss"))
          runFactWriter 160 outputFile 
              <| factWriter { 
                      do! tellComment shortName
                      do! tellComment audit
                      do! factOutput
                      return ()
                  }

    let generateAideStructRelFacts (rows : seq<AideStructRelRow>) 
                                    (outputFile : string) : unit =            
            writeFactsWithHeaderComment outputFile
                <| generatePredicates makeAideStructRelFact (Seq.toList rows)