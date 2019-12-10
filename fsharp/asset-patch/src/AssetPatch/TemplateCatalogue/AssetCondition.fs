// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplateCatalogue


[<AutoOpen>]
module AssetCondition =
    
    open AssetPatch.TemplatePatcher.Template


    /// ASSET_CONDITION
    let asset_condition : Characteristic list -> Class = 
        _class "ASSET_CONDITION"

    // ASSET_CONDITION:ConditionGrade
    type ConditionGrade = 
        | Good 
        | Fair
        | Adequate 
        | Poor
        | Awful


        override x.ToString() = 
            match x with
            | Good -> "1 - GOOD"
            | Fair -> "2 - FAIR"
            | Adequate -> "3 - ADEQUATE"
            | Poor -> "4 - Poor"
            | Awful -> "5 - Awful"

        static member TryParse (source : string) : ConditionGrade option = 
            match source with
            | null | "" -> None
            | _ -> 
                match source.ToUpper().Trim() with
                | "GOOD" -> Some Good 
                | "FAIR" -> Some Fair
                | "ADEQUATE" -> Some Adequate 
                | "POOR" -> Some Poor
                | "AWFUL" -> Some Awful 
                | _ -> None

            
    /// ASSET_CONDITION:CONDITION_GRADE
    let condition_grade (v : ConditionGrade) : Characteristic = 
        _characteristic "CONDITION_GRADE" (v.ToString())


    /// ASSET_CONDITION:CONDITION_GRADE_REASON
    let condition_grade_reason (v : string) : Characteristic = 
        _characteristic "CONDITION_GRADE_REASON" (v.ToUpper())

    /// ASSET_CONDITION:PerformanceGrade
    type PerformanceGrade = 
        | Availability_95 
        | Availability_90_94
        | Availability_80_89
        | Availability_50_79
        | Availability_49

        override x.ToString() = 
            match x with
            | Availability_95 -> "1 - AVAILABILITY 95%"
            | Availability_90_94 -> "2 - AVAILABILITY 90% - 94%"
            | Availability_80_89 -> "3 - AVAILABILITY 80% - 89%"
            | Availability_50_79 -> "4 - AVAILABILITY 50% - 79%"
            | Availability_49 -> "5 - AVAILABILITY 49%"

        static member TryParse (source : string) : PerformanceGrade option = 
            match source with
            | null | "" -> None
            | _ -> 
                match source.ToUpper().Trim() with
                | "95%" -> Some Availability_95 
                | "90% - 94%" -> Some Availability_90_94
                | "80% - 89%" -> Some Availability_80_89
                | "50% - 79%" -> Some Availability_50_79
                | "49%" -> Some Availability_49
                | _ -> None


    /// ASSET_CONDITION:PERFORMANCE_GRADE
    let performance_grade (v : PerformanceGrade) : Characteristic = 
        _characteristic "PERFORMANCE_GRADE" (v.ToString())


    /// ASSET_CONDITION:PERFORMANCE_GRADE_REASON
    let performance_grade_reason (v : string) : Characteristic = 
        _characteristic "PERFORMANCE_GRADE_REASON" (v.ToUpper())


    /// ASSET_CONDITION:LoadingFactor
    type LoadingFactor = 
        | SignificantlyUnderLoaded
        | UnderLoaded
        | Satisfactory 
        | Overloaded
        | Unacceptable

        override x.ToString() = 
            match x with
            | SignificantlyUnderLoaded -> "1 - SIGNIFICANTLY UNDER LOADED"
            | UnderLoaded -> "2 - UNDER LOADED"
            | Satisfactory -> "3 - SATISFACTORY"
            | Overloaded -> "4 - OVERLOADED"
            | Unacceptable -> "5 - UNACCEPTABLE"

        static member TryParse (source : string) : LoadingFactor option = 
            match source with
            | null | "" -> None
            | _ -> 
                match source.ToUpper().Trim() with
                | "SIGNIFICANTLY UNDER LOADED" -> Some SignificantlyUnderLoaded
                | "UNDER LOADED" -> Some UnderLoaded
                | "SATISFACTORY" -> Some Satisfactory 
                | "OVERLOADED" -> Some Overloaded
                | "UNACCEPTABLE" -> Some Unacceptable
                | _ -> None

    /// ASSET_CONDITION:LOADING_FACTOR
    let loading_factor (v : LoadingFactor) : Characteristic = 
        _characteristic "LOADING_FACTOR" (v.ToString())


    /// ASSET_CONDITION:LOADING_FACTOR_REASON
    let loading_factor_reason (v : string) : Characteristic = 
         _characteristic "LOADING_FACTOR_REASON" (v.ToUpper())


    /// ASSET_CONDITION:SURVEY_DATE
    let survey_date (year : uint32) : Characteristic = 
        _characteristic "SURVEY_DATE" (year.ToString())

    /// Emit ASSET_CONDITION with defaults for new
    let asset_condition_new_item (year : uint32) : Class = 
        asset_condition 
            [   condition_grade Good
                condition_grade_reason "New"
                performance_grade Availability_95
                performance_grade_reason "New"
                loading_factor Satisfactory
                loading_factor_reason "New"
                survey_date year
            ]