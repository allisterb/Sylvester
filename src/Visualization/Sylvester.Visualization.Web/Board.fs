namespace Sylvester

type Board(container:string, renderer:string, id:string, origin: {| usrCoords: float * float * float; scrCoords: float * float * float |}, zoomX, zoomY, unitX, unitY, canvasWidth, canvasHeight, attributes) =
    member val Container = container
    member val Renderer = renderer
    member val Id = id
    member val Origin = origin
    