// namespace tycoonService

module TycoonService.TycoonSolution
open System

    type LoadResult = Loaded | NotLoaded

    type Node = Factory |  Port | A | B
        
    type Path = {StartNode: Node; TimeToTravel: int;  EndNode: Node }
    type Moving = {Connection:Path; RelativePosition:int}
    type Position = Moving of Moving  | Static of Node

    type GoAndBackPaths = {GoPath:Path;BackPath:Path}

    type Cargo = {CargoName:string; Paths:Path list;NodeLocation:Node option}

    type Vehicle = {
        VehicleName:string; 
        Cargo: Cargo option;
        Tour: GoAndBackPaths option;
        Position:Position
        }

    type World = {Vehicles: Vehicle list; StationaryCargos:Cargo list}
    
    let standardWorld =
        {
            Vehicles = [
               {
                   VehicleName = "truck1"
                   Cargo = None
                   Tour = None
                   Position = Static Factory
               };
               {
                   VehicleName = "truck2"
                   Cargo = None
                   Tour = None
                   Position = Static Factory
               };
               {
                   VehicleName = "boat"
                   Cargo= None
                   Tour = None
                   Position = Static Port
               }
            
            ];
            StationaryCargos = []
        }
  
    let getPathFromFactoryTo (node:Node) =
        match node with
             |A ->
                [{StartNode=Factory;EndNode=Port;TimeToTravel=1};{StartNode=Port;EndNode=A;TimeToTravel=4} ]
             | B ->
                [{StartNode=Factory;EndNode=B;TimeToTravel=5}]
             | _ -> failwith (sprintf "node error: %s" (node.ToString()))
            
    let getNodeFromChar (c:char) =
        match c with
        | 'A' ->  A
        | 'B' ->  B
        | _ -> failwith (sprintf "char error: %c" c)
    
    let createCargoHavingDestination (c:char) =
        let node = getNodeFromChar c
        let path = getPathFromFactoryTo node
        let cargo = {
                     CargoName = (sprintf "%c_%s" c (Guid.NewGuid().ToString()));
                     Paths = path
                     NodeLocation = Some Factory
                     }
        cargo
    
    let createStandardWorldWithCargos  (w:string) = 
        w.ToCharArray()
        |> List.ofArray
        |> List.fold (fun acc x ->
            {
                acc with StationaryCargos =  (acc.StationaryCargos)@[(createCargoHavingDestination x)]
            }) standardWorld

    let getBackwardPath (path:Path) =
        {StartNode = path.EndNode;TimeToTravel=path.TimeToTravel;EndNode=path.StartNode}
   
    let trySetVehicleDirection (vehicle:Vehicle) =
        match vehicle.Cargo with
        | Some C ->
            match C.Paths with
            | H::_ ->
                {
                    vehicle with
                    Tour = Some {GoPath = H; BackPath=(getBackwardPath H)};
                    Position =Moving {Connection = H;RelativePosition=0};
                    Cargo = Some {C with NodeLocation=None}
                }
            | [] -> vehicle
        | _ -> vehicle
    
    let trySetVehiclesDirections (vehicles: Vehicle list) =
        vehicles |> List.map (fun x -> trySetVehicleDirection x)
        

    
    /// **Description**
    /// vehicle loads the cargo it is in the same place. 
    /// 
    /// **Parameters**
    ///   * `vehicle` - parameter of type `Vehicle`
    ///   * `cargo` - parameter of type `Cargo`
    ///
    /// **Output Type**
    ///   * `Vehicle * Cargo option`
    /// return the vehicle and the cargo changed if the cargo is loaded,
    /// otherwise returns the same vehicle and None
    ///
    /// **Exceptions**
    ///
    let tryLoadSingleCargo vehicle cargo =
         match vehicle.Cargo,cargo.NodeLocation,vehicle.Position,cargo.Paths with
         | Some _,_,_,_  -> (vehicle,None)
         | None,Some X,Static Y,H::T  when X=Y ->
             let newCargo = {cargo with Paths=T;NodeLocation=None }
             let newVehicle =
                 {vehicle with
                      Cargo = Some newCargo;
                      Position = Moving {Connection=H;RelativePosition=0};
                      Tour= Some {GoPath=H;BackPath = getBackwardPath H}
                 }
             (newVehicle,Some newCargo)
         | _ -> (vehicle,None)
         
    
    
    
    /// **Description**
    /// vehicle tries to load the first cargo that can be loaded in the list
    /// 
    /// 
    /// **Parameters**
    ///   * `vehicle` - parameter of type `Vehicle`
    ///   * `cargos` - parameter of type `Cargo list`
    ///
    /// **Output Type**
    ///   * `Vehicle * Cargo list * LoadedResult`
    /// if a cargo is loaded, then the modified vehicle is returned,
    /// with the list of the remaining cargo and a "Loaded" result
    /// otherwise the vehicle and the cargos are returned as they were, with
    /// the NotLoaded result
    /// **Exceptions**
    ///
    let  tryLoadACargo vehicle cargos =
         let rec tryLoadCargoIter vehicle cargos cargosAcc  =
             match cargos with
             | [] -> (vehicle,cargosAcc,NotLoaded)
             | H::T ->
                 let (newVehicle,cargoLoaded) = tryLoadSingleCargo vehicle H
                 match cargoLoaded with
                 | Some C ->
                     let remainingCargos =   T 
                     (newVehicle,cargosAcc@remainingCargos,Loaded)
                 | None -> tryLoadCargoIter vehicle T (H::cargosAcc)
         tryLoadCargoIter  vehicle cargos []
   
         

    
    /// **Description**
    /// a list of vehicles will try to load cargos from 
    /// another list
    /// **Parameters**
    ///   * `vehicles` - parameter of type `Vehicle list`
    ///   * `cargos` - parameter of type `Cargo list`
    ///
    /// **Output Type**
    ///   * `Vehicle list * Cargo list`
    /// the input vehicles, changed if they loaded a cargo, and the remaining cargos
    /// not loaded by any vehicle
    /// **Exceptions**
    ///
    let rec tryLoadCargos (vehicles:Vehicle list) (cargos:Cargo list) =
        match (vehicles,cargos) with
        | _,[] -> (vehicles,[])
        | H::T,_ -> let (cargoLoaded,cargosLeft,isLoaded) = tryLoadACargo H cargos
                    match isLoaded with
                    | Loaded ->
                            let (otherVehicleLoading,otherCargosLeft) = tryLoadCargos T cargosLeft
                            (cargoLoaded::otherVehicleLoading,otherCargosLeft)
                    | NotLoaded ->
                        let (otherVehicles,cargosLeft) = tryLoadCargos T cargos
                        (H::otherVehicles,cargosLeft)
        | _ -> (vehicles,cargos)                
                        
    
    
    /// **Description**
    /// a vehicle that is moved according to its tour, may have to 
    /// switch direction (if reached the end of the goPath of the tour) or
    /// has to set it from Moving to Static its position
    /// **Parameters**
    ///   * `vehicle` - parameter of type `Vehicle`
    ///
    /// **Output Type**
    ///   * `Vehicle`
    ///
    /// **Exceptions**
    ///
    let tryAlignVehiclePosition (vehicle:Vehicle)  =
        match (vehicle.Position,vehicle.Tour) with
        | Static _,_ -> vehicle
        | _,None -> vehicle
        | Moving M, Some P
            when M.Connection.TimeToTravel = M.RelativePosition && M.Connection = P.GoPath ->
            {vehicle with Position = Moving {Connection = P.BackPath; RelativePosition=0}}
        | Moving M, Some P
            when M.Connection.TimeToTravel = M.RelativePosition && M.Connection = P.BackPath ->
            {vehicle with Position = Static P.BackPath.EndNode; Tour = None}
        | _ -> vehicle    
            
            
    let tryAlignVehiclePositions (vehicles:Vehicle list) =
        vehicles |> List.map (tryAlignVehiclePosition)
    
    
    let tryDropCargo (vehicle:Vehicle) =
        match vehicle.Cargo,vehicle.Position,vehicle.Tour with
        | None,_,_ -> (vehicle,None)
        | Some C, Moving m,Some P  when m.RelativePosition = m.Connection.TimeToTravel && P.GoPath = m.Connection   ->
            ({vehicle with Cargo = None; },Some {C with NodeLocation = Some m.Connection.EndNode };)
        | Some C, Moving m,Some P  when m.RelativePosition = 0 && P.BackPath = m.Connection   ->
            ({vehicle with Cargo = None; },Some {C with NodeLocation = Some m.Connection.StartNode})
        | _ -> (vehicle,None)    
        
        

    
    /// **Description**
    /// if vehicles are where their cargos are supposed to be dropped,
    /// then they will drop them
    /// **Parameters**
    ///   * `vehicles` - parameter of type `Vehicle list`
    ///
    /// **Output Type**
    ///   * `Vehicle list * Cargo list`
    /// the vehicle after dropping, and the list of the dropped cargo will be returned
    /// **Exceptions**
    ///
    let tryDropCargos (vehicles:Vehicle list) =
        let vehiclesAndDroppedCargos = vehicles |> List.map (fun x -> tryDropCargo x)
        let vehicles = vehiclesAndDroppedCargos |> List.map (fun (x,_) -> x)
        let droppedCargos = vehiclesAndDroppedCargos |>
                            List.map (fun (_,x) -> x)
                            |> List.fold (fun acc y -> match y with | Some z -> z::acc | _ -> acc) []
        (vehicles,droppedCargos) 
            
    
    
    /// **Description**
    /// a vehicle that is moving, will update its position incrementing
    /// by 1 the "relativePosition" (considering also the case in which
    /// it will change direction)
    /// **Parameters**
    ///   * `vehicle` - parameter of type `Vehicle`
    ///
    /// **Output Type**
    ///   * `Vehicle`
    ///
    /// **Exceptions**
    ///
    let updateVehiclePosition (vehicle:Vehicle) =
        match (vehicle.Position,vehicle.Tour) with
        | (Moving M,Some P) -> 
            if (M.Connection = P.GoPath) then (
                if (M.RelativePosition<P.GoPath.TimeToTravel-1) then
                    {vehicle with Position = Moving {M with RelativePosition=M.RelativePosition+1}}
                else
                    {vehicle with Position = Moving {Connection=P.BackPath;RelativePosition=0} }
            ) else (
                if (M.RelativePosition<P.BackPath.TimeToTravel-1) then
                    {vehicle with Position = Moving {M with RelativePosition=M.RelativePosition+1}}
                else 
                    {vehicle with Position = Static P.BackPath.EndNode; Tour=None}
            )
        | _ -> vehicle
    
    let updateAllVehiclesPositions (vehicles:Vehicle list) =
        let newVehicles = vehicles |> List.map (fun x -> updateVehiclePosition x)
        newVehicles
        
    
    let tick world =
        let (newVehicles,leftCargos) = tryLoadCargos world.Vehicles world.StationaryCargos
        let newVehicles2 = updateAllVehiclesPositions newVehicles
        let (newVehicles3,droppedCargos) = tryDropCargos newVehicles2
        let newVehicles4 = tryAlignVehiclePositions newVehicles3
        {Vehicles=newVehicles4;StationaryCargos=droppedCargos@leftCargos}

        
    
    /// **Description**
    /// the number of the ticks needed for the cargos to reach their destination
    /// **Parameters**
    ///   * `world` - parameter of type `World`
    ///   * `nCargos` - parameter of type `int`
    ///
    /// **Output Type**
    ///   * `int`
    ///
    /// **Exceptions**
    ///
    let numberOfTicksForTheCargoBeingAtDestination (world:World) nCargos  =
        let newWorld = world |> tick
        let theSeq =
            (0,newWorld)
            |> Seq.unfold (fun (c,w) ->
                if (List.length
                        (w.StationaryCargos
                         |> List.filter
                                (fun x -> match x.NodeLocation with | Some X when (X = A|| X = B)  -> true | _ -> false )) =  nCargos) then
                    None
                else Some((c,w),(c+1,tick w))
            )
        Seq.length theSeq + 1




    
    /// **Description**
    /// the number of hours needed to 
    /// transport cargos directed in A or B.
    /// **Parameters**
    ///   * `w` - parameter of type `String`
    /// "AB" means that one cargo has to go in A
    /// and one cargo has to go in B
    ///
    /// **Output Type**
    ///   * `int`
    /// how long it will take until all the cargos are
    /// at their destintion
    /// **Exceptions**
    ///
    let answer (w:String) =
        let myWorld = createStandardWorldWithCargos w
        let wLength = String.length w
        numberOfTicksForTheCargoBeingAtDestination myWorld wLength
        