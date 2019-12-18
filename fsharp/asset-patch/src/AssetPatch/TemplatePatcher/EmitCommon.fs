// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher


module EmitCommon =


    open AssetPatch.TemplatePatcher.PatchTypes
    
    // ************************************************************************
    // Phase 1

    
    type NewFlocProperties = 
        { ClassFlocs : NewClassFloc list
          ValuaFlocs : NewValuaFloc list
        }
        member x.IsEmpty 
            with get () : bool = 
                x.ClassFlocs.IsEmpty && x.ValuaFlocs.IsEmpty
        
        static member ConcatNewFlocProperties (source : NewFlocProperties list) : NewFlocProperties = 
            let add (r1 : NewFlocProperties) (acc : NewFlocProperties) = 
                { ClassFlocs = r1.ClassFlocs @ acc.ClassFlocs
                  ValuaFlocs = r1.ValuaFlocs @ acc.ValuaFlocs
                }
            List.foldBack add source { ClassFlocs = []; ValuaFlocs = [] }



    type Phase1FlocData = 
        { FuncLocs : NewFuncLoc list
          FuncLocLinks : LinkFuncLoc list
          ClassFlocs : NewClassFloc list
          ValuaFlocs : NewValuaFloc list
        }
        member x.IsEmpty 
            with get () : bool = 
                x.FuncLocs.IsEmpty &&  x.ClassFlocs.IsEmpty && x.ValuaFlocs.IsEmpty

        member x.RemoveDups() : Phase1FlocData = 
            { FuncLocs = x.FuncLocs |> List.distinctBy (fun x -> x.FunctionLocation)
              FuncLocLinks = x.FuncLocLinks |> List.distinctBy (fun x -> x.FunctionLocation)
              ClassFlocs = x.ClassFlocs |> List.distinctBy (fun x -> x.FuncLoc.ToString() + "!!" + x.Class)
              ValuaFlocs = x.ValuaFlocs |> List.distinctBy (fun x -> x.FuncLoc.ToString() + "!!" + x.CharacteristicID + "!!" + x.ValueCount.ToString())
            }

        static member Concat (source : Phase1FlocData list) : Phase1FlocData = 
            { FuncLocs = source |> List.map (fun x -> x.FuncLocs) |> List.concat
              FuncLocLinks = source |> List.map (fun x -> x.FuncLocLinks) |> List.concat
              ClassFlocs = source |> List.map (fun x -> x.ClassFlocs) |> List.concat
              ValuaFlocs = source |> List.map (fun x -> x.ValuaFlocs) |> List.concat
            }


    type Phase1EquiData = 
        { Equis : NewEqui list
        }
        member x.IsEmpty 
            with get () : bool = x.Equis.IsEmpty 

        member x.RemoveDups () : Phase1EquiData = 
            { Equis = x.Equis |> List.distinctBy (fun x -> x.FuncLoc.ToString() + "!!" + x.Description)
            }
        static member Empty : Phase1EquiData = { Equis = [] }
            
        static member Concat (source : Phase1EquiData list) : Phase1EquiData = 
            let add (r1 : Phase1EquiData) (acc : Phase1EquiData) = 
                { Equis = r1.Equis @ acc.Equis }
            List.foldBack add source { Equis = [] }



    type Phase1Data = 
        { FlocData : Phase1FlocData
          EquiData : Phase1EquiData
        }

        static member Concat (xs : Phase1Data list) : Phase1Data = 
            { FlocData = xs |> List.map (fun x -> x.FlocData) |> Phase1FlocData.Concat
              EquiData = xs |> List.map (fun x -> x.EquiData) |> Phase1EquiData.Concat
            }

        member x.RemoveDups() : Phase1Data = 
            { FlocData = x.FlocData.RemoveDups()
              EquiData = x.EquiData.RemoveDups()
            }

    // ************************************************************************
    // Phase 2
    
    type Phase2EquiData = 
        { ClassEquis : NewClassEqui list
          ValuaEquis : NewValuaEqui list
        }
        member x.IsEmpty 
            with get () : bool = 
                x.ClassEquis.IsEmpty && x.ValuaEquis.IsEmpty

        static member Concat (source : Phase2EquiData list) : Phase2EquiData = 
            let add (r1 : Phase2EquiData) (acc : Phase2EquiData) = 
                { ClassEquis = r1.ClassEquis @ acc.ClassEquis
                  ValuaEquis = r1.ValuaEquis @ acc.ValuaEquis
                }
            List.foldBack add source { ClassEquis = []; ValuaEquis = [] }

    type Phase2Data = Phase2EquiData