#r "nuget: Feliz.ViewEngine, 0.24.0"
#r "nuget: Plotly.NET, 4.2.0"


open Feliz.ViewEngine
open System 
open System.Diagnostics
open System.IO
open Plotly.NET
open Plotly.NET.LayoutObjects


module Light = 
  let private colorway =
      Color.fromColors
          [|
              Color.fromHex "#6929C4"
              Color.fromHex "#1192E8"
              Color.fromHex "#005D5D"
              Color.fromHex "#9F1853"
              Color.fromHex "#EE5396"
              Color.fromHex "#002D9C"
              Color.fromHex "#520408"
              Color.fromHex "#198038"
              Color.fromHex "#FA4D56"
              Color.fromHex "#B28600"
              Color.fromHex "#009D9A"
              Color.fromHex "#001141"
              Color.fromHex "#8A3800"
              Color.fromHex "#A56EFF"
       
          |]

  let private axisTemplate =
      LinearAxis.init (
          LineColor = Color.fromHex "#8D8D8D",
          GridColor = Color.fromString "#E0E0E0",
          Ticks = StyleParam.TickOptions.Empty,
          ShowLine = true,
          ZeroLine = false,
          Title = Title.init(Standoff = 8),
          TickFont = Font.init(
            Family = StyleParam.FontFamily.Custom "IBM Plex Sans SemiBold,Times New Roman,Helvetica Neue,Arial,sans-serif",
            Size = 12.0,
            Color = Color.fromHex "#525252"
          )
      )

  let hncTemplate =
      Layout.init (
          Colorway = colorway,
          PaperBGColor = Color.fromHex "#FFFFFF",
          PlotBGColor = Color.fromHex "#FFFFFF",
          Font = Font.init(
            Family = StyleParam.FontFamily.Custom "IBM Plex Sans Regular,Times New Roman,Helvetica Neue,Arial,sans-serif",
            Size = 12.0,
            Color = Color.fromHex "#161616"
          ),
          Title = Title.init(
            Font = Font.init(
              Family = StyleParam.FontFamily.Custom "IBM Plex Sans SemiBold,Times New Roman,Helvetica Neue,Arial,sans-serif",
              Size = 16.0,
              Color = Color.fromHex "#161616"
            ),
            X = 0.05
          ),
          Legend = Legend.init(
            Font = Font.init(
              Family = StyleParam.FontFamily.Custom "IBM Plex Sans Regular,Times New Roman,Helvetica Neue,Arial,sans-serif",
              Size = 12.0,
              Color = Color.fromHex "#525252"
            )
            //Orientation = StyleParam.Orientation.Horizontal, 
            //XAnchor = StyleParam.XAnchorPosition.Center,
            //YAnchor = StyleParam.YAnchorPosition.Bottom,
            //Y = 1.0,
            //X = 0.5
          )
      )
      |> Layout.setLinearAxis ((StyleParam.SubPlotId.XAxis 1), axisTemplate)
      |> Layout.setLinearAxis ((StyleParam.SubPlotId.YAxis 1), axisTemplate)
      |> Template.init


module Dark = 
  let private colorway =
      Color.fromColors
          [|
              Color.fromHex "#8A3FFC"
              Color.fromHex "#08BDBA"
              Color.fromHex "#BAE6FF"
              Color.fromHex "#33B1FF"
              Color.fromHex "#007D79"
              Color.fromHex "#6FDC8C"
              Color.fromHex "#4589FF"
              Color.fromHex "#D4BBFF"
              Color.fromHex "#FFF1F1"
              Color.fromHex "#FF7EB6"
              Color.fromHex "#D02670"
              Color.fromHex "#FA4D56"
              Color.fromHex "#BA4E00"
              Color.fromHex "#D2A106"
          |]

  let private axisTemplate =
      LinearAxis.init (
          LineColor = Color.fromHex "#8D8D8D",
          GridColor = Color.fromString "#393939",
          Ticks = StyleParam.TickOptions.Empty,
          ShowLine = true,
          ZeroLine = false,
          Title = Title.init(Standoff = 8),
          TickFont = Font.init(
            Family = StyleParam.FontFamily.Custom "IBM Plex Sans SemiBold,Times New Roman,Helvetica Neue,Arial,sans-serif",
            Size = 12.0,
            Color = Color.fromHex "#525252"
          )
      )

  let hncTemplate =
      Layout.init (
          Colorway = colorway,
          PaperBGColor = Color.fromHex "#001141",
          PlotBGColor = Color.fromHex "#001141",
          Font = Font.init(
            Family = StyleParam.FontFamily.Custom "IBM Plex Sans Regular,Times New Roman,Helvetica Neue,Arial,sans-serif",
            Size = 12.0,
            Color = Color.fromHex "#F4F4F4"
          ),
          Title = Title.init(
            Font = Font.init(
              Family = StyleParam.FontFamily.Custom "IBM Plex Sans SemiBold,Times New Roman,Helvetica Neue,Arial,sans-serif",
              Size = 16.0,
              Color = Color.fromHex "#F4F4F4"
            ),
            X = 0.05
          ),
          Legend = Legend.init(
            Font = Font.init(
              Family = StyleParam.FontFamily.Custom "IBM Plex Sans Regular,Times New Roman,Helvetica Neue,Arial,sans-serif",
              Size = 12.0,
              Color = Color.fromHex "#525252"
            )
            //Orientation = StyleParam.Orientation.Horizontal, 
            //XAnchor = StyleParam.XAnchorPosition.Center,
            //YAnchor = StyleParam.YAnchorPosition.Bottom,
            //Y = 1.0,
            //X = 0.5
          )
      )
      |> Layout.setLinearAxis ((StyleParam.SubPlotId.XAxis 1), axisTemplate)
      |> Layout.setLinearAxis ((StyleParam.SubPlotId.YAxis 1), axisTemplate)
      |> Template.init
let displayCharts (charts: GenericChart.GenericChart seq) = 

    let build charts = 
        Html.html [
            Html.head [ Html.title "Charts" ]
            Html.body [
                for c in (charts|> Seq.rev) do 
                    prop.dangerouslySetInnerHTML (c |> Chart.withTemplate Light.hncTemplate |> GenericChart.toEmbeddedHTML)
                ]
            ]
    
    let document = Render.htmlDocument (build charts)
    let fileName = sprintf "%s%s%s" (Path.GetTempPath()) (Guid.NewGuid().ToString()) ".html"
    File.WriteAllText(fileName, document)
    let  psi = new ProcessStartInfo(UseShellExecute = true,  FileName = fileName)
    let p = new Process(StartInfo = psi)
    p.Start()