module TycoonService.UnitTestFinal
open TycoonService.TycoonSolution

open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

[<Test>]
let aWorldWithNoVehicleAndNoContainerWillNotChange() =
    // given
    let world = {Vehicles=[];StationaryCargos=[]}
    //when
    let actualNextWorld = tick world
    // then
    let expectedNextWorld = {Vehicles=[];StationaryCargos=[]}
    Assert.AreEqual(expectedNextWorld,actualNextWorld)
    
    
[<Test>]
let ``vehicle doesn't load any cargo because there aren't any of them, so the world stay the same``()=
    // given
    let vehicle = {
        VehicleName="truck"
        Cargo = None
        Position = Static Factory
        Tour = None
    }
    let world = {Vehicles = [vehicle];StationaryCargos=[]}
    // when
    let nextWorld = tick world
    
    // then
    Assert.AreEqual(world,nextWorld)

    
    
    
[<Test>]
let ``a vehicle will load a cargo if they are in the same node and  the cargo has a destination ``()=
    // given
    let factoryToBPath = {StartNode=Factory;TimeToTravel=4;EndNode=A}
    let cargoWaitingForATruck = {CargoName="cargoWaiting";Paths=[factoryToBPath];NodeLocation = Some Factory}
    let vehicle = {
        VehicleName="truck"
        Cargo = None
        Position = Static Factory
        Tour = None
    }
    let world = {Vehicles = [vehicle];StationaryCargos=[cargoWaitingForATruck]}
    
    // when
    let actualNextWorld = tick world
    
    // then
    let bToFactoryPath= {StartNode=A;TimeToTravel=4;EndNode=Factory}
    let truckAfterLoadingTheCargo =
        {
            VehicleName="truck"
            Cargo = Some {cargoWaitingForATruck with Paths=[];NodeLocation=None}
            Position = Moving {Connection=factoryToBPath;RelativePosition=1}
            Tour = Some {GoPath=factoryToBPath;BackPath=bToFactoryPath}
        }
        
    let expectedNewWorld = {Vehicles = [truckAfterLoadingTheCargo];StationaryCargos=[]}
    Assert.AreEqual(expectedNewWorld,actualNextWorld)
    
    
    
    
[<Test>]
let ``two step vehicle moving``()=
    // given
    let vehicle = {VehicleName="vehicle"; Cargo=None;Position = Static Factory;Tour=None}
    let cargo = {CargoName="cargo";Paths = [{StartNode=Factory;EndNode=B;TimeToTravel=2}];NodeLocation=Some Factory}
    let world = {Vehicles=[vehicle];StationaryCargos=[cargo]}
    
    // when
    let newWorld = world |> tick |> tick
    
    // then
    let expectedCargoDropped = {cargo with NodeLocation= Some B;Paths= []}
    let expectedVehicle = {vehicle with
                           Position = Moving {Connection={StartNode=B;TimeToTravel=2;EndNode=Factory};
                           RelativePosition =0}
                           Tour = Some {
                               GoPath= {StartNode=Factory;TimeToTravel=2;EndNode=B};
                               BackPath={StartNode=B;TimeToTravel=2;EndNode=Factory}}
                           }
    
    match newWorld.StationaryCargos with
    | H::_ -> Assert.AreEqual(expectedCargoDropped,H)
    | _ -> Assert.Fail()
    
    match newWorld.Vehicles with
    | H::_ -> Assert.AreEqual(expectedVehicle,H)
    | _ -> Assert.Fail()
    
 
[<Test>]
let ``two steps to drop the cargo at destination, and other two steps to go back to the factory``()=
    // given
    let vehicle = {VehicleName="vehicle"; Cargo=None;Position = Static Factory;Tour=None}
    let cargo = {CargoName="cargo";Paths = [{StartNode=Factory;EndNode=B;TimeToTravel=2}];NodeLocation=Some Factory}
    let world = {Vehicles=[vehicle];StationaryCargos=[cargo]}
    
    // when
    let newWorld = world |> tick |> tick |> tick |> tick
    
    // then
    let expectedCargoDropped = {cargo with NodeLocation= Some B;Paths= []}
    let expectedVehicle = {vehicle with
                               Position = Static Factory
                               Tour = None
                           }
    
    match newWorld.StationaryCargos with
    | H::_ -> Assert.AreEqual(expectedCargoDropped,H)
    | _ -> Assert.Fail()
    
    match newWorld.Vehicles with
    | H::_ -> Assert.AreEqual(expectedVehicle,H)
    | _ -> Assert.Fail()
    
 
[<Test>]
let ``a cargo is directed to B, and in two steps will be dropped to the port``()=
    // given
    let factoryToPortPath =  {StartNode=Factory;EndNode=Port;TimeToTravel=2}
    let portToBPath =  {StartNode=Port;EndNode=B;TimeToTravel=2}
    
    let vehicle = {VehicleName="vehicle"; Cargo=None;Position = Static Factory;Tour=None}
    let cargo = {CargoName="cargo"; Paths = [factoryToPortPath;portToBPath]; NodeLocation = Some Factory }
    let world = {Vehicles=[vehicle];StationaryCargos=[cargo]}
    
    // when
    let newWorld = world |> tick  |> tick 
    
    // then
    let expectedCargoDropped = {cargo with NodeLocation= Some Port;Paths= [portToBPath]}
    
    match newWorld.StationaryCargos with
    | H::_ -> Assert.AreEqual(expectedCargoDropped,H)
    | _ -> Assert.Fail()
    
    
 
[<Test>]
let ``a cargo is dropped at a distance 2, and so the vehicle is back at factory in 4 steps``()=
    // given
    let factoryToPortPath =  {StartNode=Factory;EndNode=Port;TimeToTravel=2}
    let portToBPath =  {StartNode=Port;EndNode=B;TimeToTravel=2}
    
    let vehicle = {VehicleName="vehicle"; Cargo=None;Position = Static Factory;Tour=None}
    let cargo = {CargoName="cargo"; Paths = [factoryToPortPath;portToBPath]; NodeLocation = Some Factory }
    let world = {Vehicles=[vehicle];StationaryCargos=[cargo]}
    
    // when
    let newWorld = world |> tick  |> tick |> tick |> tick
    
    // then
    let expectedCargoDropped = {cargo with NodeLocation= Some Port;Paths= [portToBPath]}
    let expectedVehicle = {vehicle with
                               Position = Static Factory
                               Tour = None
                           }
    
    match newWorld.StationaryCargos with
    | H::_ -> Assert.AreEqual(expectedCargoDropped,H)
    | _ -> Assert.Fail()
    
    match newWorld.Vehicles with
    | H::_ -> Assert.AreEqual(expectedVehicle,H)
    | _ -> Assert.Fail()
    
    
    
    
[<Test>]
let ``a cargo directed to B is dropped to the port by the vehicle and there will be the boat bringing it to B ``()=
    // given
    let factoryToPortPath =  {StartNode=Factory;EndNode=Port;TimeToTravel=2}
    let portToBPath =  {StartNode=Port;EndNode=B;TimeToTravel=2}
    
    let vehicle = {VehicleName="vehicle"; Cargo=None;Position = Static Factory;Tour=None}
    let boat =  {VehicleName="boat"; Cargo=None;Position = Static Port;Tour=None}
    let cargo = {CargoName="cargo"; Paths = [factoryToPortPath;portToBPath]; NodeLocation = Some Factory }
    let world = {Vehicles=[vehicle;boat];StationaryCargos=[cargo]}
    
    // when
    let newWorld = world |> tick  |> tick |> tick |> tick
    
    // then
    let expectedCargoDropped = {cargo with NodeLocation= Some B;Paths= []}
    
    match newWorld.StationaryCargos with
    | H::_ -> Assert.AreEqual(expectedCargoDropped,H)
    | _ -> Assert.Fail()

    
[<Test>]
let ``two cargos directed to the port will be served in parallel, so in two ticks they are both at the destination``()=
    // given
    let factoryToPortPath =  {StartNode=Factory;EndNode=Port;TimeToTravel=2}
    
    let vehicle1 = {VehicleName="vehicle1"; Cargo=None;Position = Static Factory;Tour=None}
    let vehicle2 =  {VehicleName="vehicle2"; Cargo=None;Position = Static Factory;Tour=None}
    let cargo1 = {CargoName="cargo1"; Paths = [factoryToPortPath]; NodeLocation = Some Factory }
    let cargo2 = {CargoName="cargo2"; Paths = [factoryToPortPath]; NodeLocation = Some Factory }
    let world = {Vehicles=[vehicle1;vehicle2];StationaryCargos=[cargo1;cargo2]}
    
    // when
    let newWorld = world |> tick  |> tick
    
    // then
    let cargosLocations = newWorld.StationaryCargos |> List.map (fun x -> x.NodeLocation)
    
    Assert.AreEqual([Some Port; Some Port],cargosLocations)
    

[<Test>]


let ``two cargos, one vehicle have pick up one, bring it, and then goes back to take the other cargo``()=
    // given
    let factoryToPortPath =  {StartNode=Factory;EndNode=Port;TimeToTravel=2}
    
    let vehicle1 = {VehicleName="vehicle1"; Cargo=None;Position = Static Factory;Tour=None}
    let cargo1 = {CargoName="cargo1"; Paths = [factoryToPortPath]; NodeLocation = Some Factory }
    let cargo2 = {CargoName="cargo2"; Paths = [factoryToPortPath]; NodeLocation = Some Factory }
    let world = {Vehicles=[vehicle1];StationaryCargos=[cargo1;cargo2]}
    
    // when
    let newWorld = world |> tick  |> tick
    
    
    // then
    let cargosLocations = newWorld.StationaryCargos |> List.map (fun x -> x.NodeLocation) |> Set.ofList
    Assert.AreEqual([Some Factory; Some Port] |> Set.ofList ,cargosLocations)

        
    let newNewWorld = newWorld |> tick |> tick |> tick |> tick     
    let newCargosLocations = newNewWorld.StationaryCargos |> List.map (fun x -> x.NodeLocation) 
    
    Assert.AreEqual([Some Port;Some Port],newCargosLocations)
    
    
            

[<Test>]
let ``single cargo that is somewhere else will not be loaded`` ()=
    // given
    let vehicle =
        {
            VehicleName="vehicle"
            Tour=None
            Position=Static Factory
            Cargo = None
        }
    let cargo = {CargoName="cargo";Paths=[]; NodeLocation = Some Port}
    
    // when
    let (vehicleTryingLoadingCargo,loadedCargo) = tryLoadSingleCargo vehicle cargo
    
    // then
    Assert.AreEqual(vehicle,vehicleTryingLoadingCargo)
    Assert.AreEqual(None,loadedCargo)
    
    
    
     
[<Test>]
let ``vehicle in factory will not load any cargo because they are to the port ``()=
    // given
    let factoryToPortPath = {StartNode=Factory;TimeToTravel=2;EndNode=Port}
    let vehicle =
        {
            VehicleName="vehicle"
            Tour=None
            Position=Static Factory
            Cargo = None
        }
        
    let unloadableCargo1 = {CargoName="unloadableCargo1";Paths=[factoryToPortPath]; NodeLocation = Some Port}
    let unLoadableCargo2 = {CargoName="unLoadableCargo2";Paths=[factoryToPortPath]; NodeLocation = Some Port}
    
    // when
    let (vehicleLoadACargo,cargoLeft,_) = tryLoadACargo vehicle [unloadableCargo1;unLoadableCargo2]
    
    // then
    Assert.AreEqual(vehicle,vehicleLoadACargo)
    Assert.AreEqual([unLoadableCargo2;unloadableCargo1] |> Set.ofList, cargoLeft |> Set.ofList)
    

    
[<Test>]
let ``one vehicle and some cargo, one of them can be loaded by the vehicle``()=
    // given
    let factoryToPortPath = {StartNode=Factory;TimeToTravel=2;EndNode=Port}
    let vehicle =
        {
            VehicleName="vehicle"
            Tour=None
            Position=Static Factory
            Cargo = None
        }
        
    let loadableCargo = {CargoName="loadableCargo";Paths=[factoryToPortPath]; NodeLocation = Some Factory}
    let unLoadableCargo = {CargoName="unLoadableCargo";Paths=[factoryToPortPath]; NodeLocation = Some Port}
    
    // when
    let (_,cargoLeft) = tryLoadCargos [vehicle] [loadableCargo;unLoadableCargo]
    
    // then

    Assert.AreEqual([unLoadableCargo],cargoLeft)
    
    
    
[<Test>]
let ``one vehicle load one cargo choosen among two`` ()=
    let factoryToPortPath = {StartNode=Factory;TimeToTravel=2;EndNode=Port}
    let vehicle =
        {
            VehicleName="vehicle"
            Tour=None
            Position=Static Factory
            Cargo = None
        }
        
    let loadableCargo = {CargoName="loadableCargo";Paths=[factoryToPortPath]; NodeLocation = Some Factory}
    let unLoadableCargo = {CargoName="unLoadableCargo";Paths=[factoryToPortPath]; NodeLocation = Some Port}
    let cargoList = [loadableCargo;unLoadableCargo]
    
    let (_,c,_) = tryLoadACargo vehicle cargoList
    
    Assert.AreEqual(1,List.length c)
    

    
    
[<Test>]
let ``a cargo with two paths, one to the port, an another to the A destination`` ()=
    let factoryToPortPath = {StartNode=Factory;TimeToTravel=2;EndNode=Port}
    let portToAPath = {StartNode=Port;TimeToTravel=2;EndNode=A}
    let cargo =
        {
                  CargoName="cargoName"
                  Paths = [factoryToPortPath;portToAPath] 
                  NodeLocation = Some Factory
        }
    let truck =
        { VehicleName = "truck";
          Cargo = None
          Tour = None
          Position = Static Factory
        }
        
    let boat =
        { VehicleName = "boat";
          Cargo = None
          Tour = None
          Position = Static Port
        }
    
    let world = {Vehicles = [truck;boat];StationaryCargos=[cargo]}
    
    let newWorld = world |> tick |> tick |> tick |> tick
    Assert.AreEqual(1,List.length newWorld.StationaryCargos)
    match newWorld.StationaryCargos with
    | H::_ -> Assert.AreEqual(Some A, H.NodeLocation)
    | _ -> Assert.Fail()
   
     
    
[<Test>]
let ``cargo goes to A in five steps`` ()=
    let factoryToPortPath = {StartNode=Factory;TimeToTravel=1;EndNode=Port}
    let portToAPath = {StartNode=Port;TimeToTravel=4;EndNode=A}
    let cargo =
        {
                  CargoName="cargoName"
                  Paths = [factoryToPortPath;portToAPath] 
                  NodeLocation = Some Factory
        }
    let truck =
        { VehicleName = "truck";
          Cargo = None
          Tour = None
          Position = Static Factory
        }
        
    let boat =
        { VehicleName = "boat";
          Cargo = None
          Tour = None
          Position = Static Port
        }
    
    let world = {Vehicles = [truck;boat];StationaryCargos=[cargo]}
    
    let newWorld = world |> tick |> tick |> tick |> tick |> tick
    Assert.AreEqual(1,List.length newWorld.StationaryCargos)
    match newWorld.StationaryCargos with
    | H::_ -> Assert.AreEqual(Some A, H.NodeLocation)
    | _ -> Assert.Fail()
   

    
[<Test>]
let ``cargo goes to A and B in five steps`` ()=
    let factoryToPortPath = {StartNode=Factory;TimeToTravel=1;EndNode=Port}
    let factoryToBPath = {StartNode=Factory;TimeToTravel=5;EndNode = B}
    let portToAPath = {StartNode=Port;TimeToTravel=4;EndNode=A}
    let cargo1 =
        {
                  CargoName="cargo1"
                  Paths = [factoryToPortPath;portToAPath] 
                  NodeLocation = Some Factory
        }
    let cargo2 =
        {
                  CargoName="cargo2"
                  Paths = [factoryToBPath] 
                  NodeLocation = Some Factory
        }
        
    let truck =
        { VehicleName = "truck";
          Cargo = None
          Tour = None
          Position = Static Factory
        }
    let truck2 =
        { VehicleName = "truck2";
          Cargo = None
          Tour = None
          Position = Static Factory
        }
        
    let boat =
        { VehicleName = "boat";
          Cargo = None
          Tour = None
          Position = Static Port
        }
    
    let world = {Vehicles = [truck;truck2;boat];StationaryCargos=[cargo1;cargo2]}
    
    let newWorld = world |> tick |> tick |> tick |> tick |> tick
    
    printf "%s\n" (newWorld.ToString())
    
    Assert.AreEqual(2,List.length newWorld.StationaryCargos)
    
    


[<Test>]
let ``make a standard world with no cargos``()=
    let destString = ""
    let worldWithCargos = createStandardWorldWithCargos destString
    
    Assert.AreEqual(standardWorld,worldWithCargos)
    
[<Test>]
let ``make a standard world with a cargo having A as destination``()=
    let destString = "A"
    let worldWithCargos = createStandardWorldWithCargos destString
    
    Assert.AreEqual(1,List.length worldWithCargos.StationaryCargos)

    
[<Test>]
let ``make a standard world with a cargo having AB as destination``()=
    let destString = "AB"
    let worldWithCargos = createStandardWorldWithCargos destString
    
    Assert.AreEqual(2,List.length worldWithCargos.StationaryCargos)
    

[<Test>]
let ``standard case 1: A`` ()=
    Assert.AreEqual(5, answer "A")
    
[<Test>]
let ``standard case 2: AB`` ()=
    Assert.AreEqual(5, answer "AB")
    
    
[<Test>]
let ``standard case 3: BB`` ()=
    Assert.AreEqual(5, answer "BB")
    
[<Test>]
let ``standard case 4: ABB`` ()=
    Assert.AreEqual(7, answer "ABB")


[<Test>]
let ``ABB complete example`` ()=
    Assert.AreEqual(41,answer "ABBBABAAABBB")
    
    
