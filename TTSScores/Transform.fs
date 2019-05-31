module Transform
open Vector

type Transform = { 
    Translation: float32*float32*float32
    Rotation: float32*float32*float32
    Scale: float32*float32*float32
} 

let rotate (rotx, roty, rotz) = rotateZ rotz>> rotateX rotx>> rotateY roty

let undoRotation (rotx, roty, rotz)  = rotateY -roty >> rotateX -rotx >> rotateZ -rotz



type Transform with
    member this.Apply = scaleComponents this.Scale  
                        >> rotate this.Rotation
                        >> translate this.Translation

    member this.Reverse = translate (scale -1.0f this.Translation)
                          >> undoRotation this.Rotation
                          >> shrinkComponents this.Scale
    member this.Contains = this.Reverse >> inUnitCube

let unitCube = {
        Translation=origin;
        Rotation=origin;
        Scale=(1.0f, 1.0f, 1.0f)
        }

let rec corners transform = 
    match transform with 
        |x when x.Equals(unitCube) -> [|
                (0.5f   ,0.5f   ,0.5f);
                (-0.5f  ,0.5f   ,0.5f);
                (0.5f   ,-0.5f  ,0.5f);
                (0.5f   ,0.5f   ,-0.5f);
                (0.5f   ,-0.5f  ,-0.5f);
                (-0.5f  ,-0.5f  ,-0.5f);
                (-0.5f  ,+0.5f  ,-0.5f);
                (-0.5f  ,-0.5f  ,0.5f);
                |]
        | _-> (corners unitCube |> Array.map transform.Apply)

type Transform with 
    member this.Corners = corners this
