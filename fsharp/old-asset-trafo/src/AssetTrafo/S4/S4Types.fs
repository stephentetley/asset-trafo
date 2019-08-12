// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo


module S4Types =
    
    open AssetTrafo.Base.Attributes
    
    type S4Component =
        { ComponentFloc            : string
          ComponentName            : string
          ComponentAttributes      : Attributes
        }

    type S4MainItem =
        { MainItemFloc            : string
          MainItemName            : string
          MainItemAttributes      : Attributes
          MainItemKids            : S4Component list
        }

    type S4Subsystem = 
        { SubsystemFloc            : string
          SubsystemName            : string
          SubsystemAttributes      : Attributes
          SubsystemKids            : S4MainItem list
        }
    
    type S4System =
        { SystemFloc               : string
          SystemName               : string
          SystemAttributes         : Attributes
          SystemKids               : S4Subsystem list
        }


    type S4Process = 
        { ProcessFloc              : string
          ProcessName              : string
          ProcessAttributes        : Attributes
          ProcessKids              : S4System list
        }

    type S4ProcessGroup = 
        { ProcessGroupFloc        : string
          ProcessGroupName        : string
          ProcessGroupAttributes  : Attributes
          ProcessGroupKids        : S4Process list
        }


    type S4Function = 
        { FunctionFloc             : string
          FunctionName             : string
          FunctionAttributes       : Attributes
          FunctionKids             : S4ProcessGroup list
        }

    type S4Site = 
        { SiteFloc                 : string
          SiteName                 : string
          SiteAttributes           : Attributes
          SiteKids                 : S4Function list
        }