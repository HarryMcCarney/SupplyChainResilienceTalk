#r "nuget: Graphoscope, 0.2.0"
#r "nuget: Cytoscape.NET, 0.2.0"
#r "nuget: FSharp.Stats, 0.5.0"
#r "nuget: Feliz.ViewEngine, 0.24.0"
#load "HnCTemplate.fsx"


module GraphGenerators = 
    open Graphoscope
    open Graphoscope.Graph
    open System

    let rng = System.Random()

    let createErdosRenyi nodes edges = 
        let g = 
            UndirectedGraph.empty
            |> UndirectedGraph.addNodes (Array.init nodes id)
            
        let createRandomEdge (nodeCount: int)=
                let f = rng.Next(nodeCount)
                let s = rng.Next(nodeCount)
                Math.Min(f,s), Math.Max(f,s)

        Seq.initInfinite(fun _ -> createRandomEdge nodes)
        |> Seq.filter(fun (o,t) -> o<>t)
        |> Seq.distinct
        |> Seq.take edges
        |> Seq.map(fun (f,t)-> f,t,1.0)
        |> Seq.toArray
        |> fun e -> UndirectedGraph.addEdges e g

    let createCompleteGraph nodesToCreate = 

        let nodeIds = Array.init nodesToCreate id
        let g = 
            UndirectedGraph.empty   
            |> UndirectedGraph.addNodes (nodeIds |> Array.map(fun i -> i))

        nodeIds |> Seq.allPairs nodeIds
        |> Seq.filter(fun (o,t) -> o<>t)
        |> Seq.map(fun (f,t)-> f,t,1.0)
        |> Seq.toArray
        |> fun e -> UndirectedGraph.addEdges e g

    let createBarabasiAlbert m nodesToAdd = 
        let normalize (ar: float []) =
            let tot = ar |> Array.sum
            ar |> Array.map(fun x -> x / tot)

        let inline cumulativeSum (ar: _ []) =
            let mutable sum = LanguagePrimitives.GenericZero
            ar |> Array.map(fun x ->
                sum <- sum + x
                sum
            )

        let chooseIndexWeighted (degrees: int []) =
            let chance = rng.NextDouble()
            degrees
            |> Array.map float
            |> normalize
            |> cumulativeSum
            |> Array.findIndex(fun x -> chance <= x)

        let chooseRandomNodeWeightedByDegree (graph: UndirectedGraph<int, float>) =
            let chance = rng.NextDouble()
            let nodes =  graph |> UndirectedGraph.getNodes

            nodes
            |>Array.map (fun node ->
                UndirectedGraph.getEdges node graph
                |> Array.length
            )
            |> Array.map float
            |> normalize
            |> cumulativeSum
            |> Array.findIndex(fun x -> chance <= x)
            |> fun ix -> nodes[ix]

        let addEdgesForNode m newNodeId (graph: UndirectedGraph<int, float>) =
            UndirectedGraph.addNode newNodeId graph |> ignore
            
            Seq.initInfinite( fun _ -> chooseRandomNodeWeightedByDegree graph)
            |> Seq.distinct
            |> Seq.take m
            |> Seq.map(fun linkNodeId -> newNodeId, linkNodeId, 1.0 )
            |> Seq.toArray
            |> fun newEdges -> UndirectedGraph.addEdges newEdges graph
            
        
        let rec addNodesandedges m nodes counter  (graph: UndirectedGraph<int, float>) = 
            let maxId = UndirectedGraph.getNodes graph |> Array.max
            addEdgesForNode m (maxId+1) graph |> ignore
            let newCounter = counter + 1
            if nodes > newCounter then 
                addNodesandedges m nodes newCounter graph
                else graph

        addNodesandedges m nodesToAdd 0 (createCompleteGraph (m))


    let createGilbert (numberOfNodes: int) (probability: float) =
            if probability > 1. || probability < 0. then failwithf "The stated probability %F is outside the expected range of 0. to 1." probability

            let rnd = new System.Random()
            let g   :  UndirectedGraph<int, float> = UndirectedGraph.empty

            for i=0 to (numberOfNodes-1) do          
                for ii=i to (numberOfNodes-1) do
                    if rnd.NextDouble() < probability then
                        g |> UndirectedGraph.addElement i i ii ii probability |> ignore
            g       


    let createRing (n: int) (k: int) =
        // Following standard practice,
        // odd values of k will be rounded down to the previous even integer.
        let g:  UndirectedGraph<int, float> =
            UndirectedGraph.empty
            |> UndirectedGraph.addNodes [|0 .. n-1|]

        // Connect each node to its half-k succeeding neighbors
        for i in 0 .. n-1 do
            for j in i+1 .. i+(k/2) do
                UndirectedGraph.addEdge (i, j%n, 1.) g|>ignore
        g

module GraphoscopeExtensions = 

    open System.Collections.Generic    
    open Graphoscope
    open Graphoscope.Graph
    open Graphoscope.FGraph
    open Graphoscope.Algorithms

    /// UndirectedGraph
    let undirectedDFS (starting : 'NodeKey) (graph : UndirectedGraph<'NodeKey, 'EdgeData>) =
            let visited = HashSet<'NodeKey>()
            let stack = Stack<'NodeKey>()

            stack.Push(starting)
            visited.Add(starting) |> ignore

            seq {
                while stack.Count > 0 do
                    let nodeKey = stack.Pop()            
                    let s = (UndirectedGraph.getEdges nodeKey graph) |> Array.map(fun (t, _) -> t)
                    yield (nodeKey)

                    for kv in s do
                        if not(visited.Contains(kv)) then
                            stack.Push(kv)
                            visited.Add(kv) |> ignore
            }

    let getDegreeDistribution (graph: UndirectedGraph<int, float>)  = 
        graph
        |> UndirectedGraph.getNodes
        |> Array.map(fun k -> UndirectedGraph.getEdges k graph |> Array.length ) 
        |> Array.sort 

    let getComponents (network: UndirectedGraph<'NodeKey, 'EdgeData>) = 
        network 
        |> UndirectedGraph.getNodes
        |> Array.map(fun k -> undirectedDFS k network |> Set.ofSeq)
        |> Set.ofSeq

    let getLargestComponentSize (network: UndirectedGraph<'NodeKey,  'EdgeData>) = 
        network
        |> getComponents
        |> Set.map(fun s -> s.Count)
        |> Set.toSeq
        |> Seq.max


    let getShortestPath (starting : 'NodeKey) (graph :  UndirectedGraph<'NodeKey, 'EdgeData> ) =
            let distance = Dictionary<'NodeKey, float>()
            let priorityQueue = SortedSet<'NodeKey * float>(Comparer<'NodeKey * float>.Create(fun (_, d1) (_, d2) -> compare d1 d2))
            let infinity = System.Double.MaxValue

            // Initialize distances to infinity for all nodes except the starting node
            // TODO: this can be improved by getOrDefault
            for nodeKey in UndirectedGraph.getNodes graph  do
                if nodeKey = starting then
                    distance.[nodeKey] <- 0.
                else
                    distance.[nodeKey] <- infinity

            priorityQueue.Add((starting, 0)) |> ignore

            while priorityQueue.Count > 0 do
                let (currentNode, currentDistance) = priorityQueue.Min
                priorityQueue.Remove(priorityQueue.Min) |> ignore
            
                let predecessors = UndirectedGraph.getEdges currentNode graph  |> Array.map(fun (p, w) -> p) 

                for kv in predecessors do
                    let totalDistance = (currentDistance + 1.0) 
                    if totalDistance < distance.[kv] then
                        distance.[kv] <- totalDistance
                        priorityQueue.Add((kv, totalDistance)) |> ignore
            

            distance

    ///FGraph
    let DFSUndirectedFGraph
        (starting : 'NodeKey) (graph : FGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        let visited = HashSet<'NodeKey>()
        let stack = Stack<'NodeKey>()

        stack.Push(starting)
        visited.Add(starting) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey = stack.Pop()            
                let (p, nd, s) = graph.[nodeKey]

                let undirectedNeighbours = p |> Seq.append s |> Seq.distinct

                yield (nodeKey, nd)

                for kv in undirectedNeighbours do
                    if not(visited.Contains(kv.Key)) then
                        stack.Push(kv.Key)
                        visited.Add(kv.Key) |> ignore
        }

    let hasGiantComponent  (network: FGraph<'NodeKey, 'NodeData, 'EdgeData>) =  
        if  (DFS.ofFGraph (network |> Seq.head).Key network
            |> Seq.length) = network.Count then true
                else false

    let getComponentsFGraph (network: FGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
        network 
        |> Seq.map(fun (KeyValue(k,v)) -> DFSUndirectedFGraph k network |> Set.ofSeq)
        |> Set.ofSeq

    let getLargestComponentSizeFGraph (network: FGraph<'NodeKey, 'NodeData, 'EdgeData>)= 
        network
        |> getComponentsFGraph
        |> Set.map(fun s -> s.Count)
        |> Set.toSeq
        |> Seq.max


    let dijkstraComputeAllPairs (graph: UndirectedGraph<int, float>): int [] * float [][] =
        // Must use 0-indexed integers for Nodekey
        let toAdjacencyMatrix (graph: UndirectedGraph<int, float>) =
            let size = UndirectedGraph.getNodes graph |> Array.length
            let matrix = Array.init size (fun _ -> Array.init size (fun _ -> 0.))
            UndirectedGraph.getAllEdges graph
            |> Array.iter(fun (s, t, v) ->
                    matrix[s][t] <- v
                    matrix[t][s] <- v
            )
            matrix

        let allDists = toAdjacencyMatrix graph
        let size = UndirectedGraph.getNodes graph |> Array.length
        
        allDists
        |> Array.iteri(fun ri r ->
            r
            |> Array.iteri(fun ci c ->
                if c = 0. && ri <> ci then
                    allDists[ri][ci] <- infinity
                elif ri = ci then
                    allDists[ri][ci] <- 0.
            )
        )
    
        let dijkstra (sourceIx: int) =
            let que= ResizeArray()
            let dist = allDists[sourceIx] |> Array.copy

            for n in 0 .. size - 1 do
                que.Add(n)

            while que.Count > 0 do
                let minDistNode = 
                    que
                    |> Seq.minBy( fun n -> dist[n])

                let minDistNodeIx =  que.IndexOf minDistNode
                que.RemoveAt minDistNodeIx

                for n in que do
                    let newCost = dist[minDistNode] + allDists[minDistNode][n]
                    if newCost < dist[n] then
                        dist[n] <- newCost
            dist
        UndirectedGraph.getNodes graph,
        [|0 .. size - 1|]
        |> Array.Parallel.map dijkstra
        

    let getGiantComponent (graph : UndirectedGraph<'NodeKey, 'EdgeData>) = 
        let gcNodes = 
            getComponents graph 
            |> Seq.maxBy(fun s -> s.Count)
            |> Set.toArray
            |> Array.mapi(fun i n -> n , i )
            |> Map

        
        let edges = 
            UndirectedGraph.getAllEdges graph
            |> Array.filter(fun (f,t,w) -> gcNodes |> Map.containsKey f &&  gcNodes |> Map.containsKey t)
            |> Array.map(fun (f,t,w) -> gcNodes[f], gcNodes[t], w)

        UndirectedGraph.empty
        |> UndirectedGraph.addNodes (gcNodes |> Seq.map(fun (KeyValue(k,v)) -> v) |> Seq.toArray)
        |> UndirectedGraph.addEdges edges
     


    let averagePathLength (dijkstraOutput: int [] * float [] []) =
    // https://networkx.org/documentation/networkx-1.3/reference/generated/networkx.average_shortest_path_length.html

        let m = dijkstraOutput |> snd
        
        m
        |> Array.map(fun x -> x |> Array.map(fun x -> if x = infinity then 0. else x)|> Array.sum)
        |> Array.sum
        |> fun x -> x / float (m.Length * (m.Length - 1))
        
module SupplyNetwork = 
    open Graphoscope
    open Graphoscope.FGraph
    open Graphoscope.Graph
    open System
    
    type NodeType = Supply of float | Demand of float 

    let rng = System.Random()

    let getWeight node (network: FGraph<'NodeKey,NodeType,'float>)  =
        let _, nd, _ = network[node] 
        match nd with 
        | Supply w -> w
        | Demand w -> w

    let getOutgoingEdgesWeight node (network: FGraph<'NodeKey,'NodeType,float>) = 
        let _, _, s = network[node]
        s
        |> Seq.sumBy(fun(KeyValue(_,w)) -> w)

    let getIncomingEdgesWeight node (network: FGraph<'NodeKey,'NodeType,float>) = 
        let p, _, _ = network[node]
        p
        |> Seq.sumBy(fun(KeyValue(_,w)) -> w)

    let availableCapcity supplyNode demandNode (network: FGraph<'NodeKey,NodeType,float>) =
        let capacity = (getWeight supplyNode network ) - (getOutgoingEdgesWeight supplyNode network)
        let deficit = (getWeight demandNode network) - (getIncomingEdgesWeight demandNode network)
        Math.Min(deficit, capacity)

    let getRandomNode (network: UndirectedGraph<'NodeKey, 'EdgeData>)  =  
        let rnd = System.Random() 
        network
        |> UndirectedGraph.getNodes
        |> fun arr ->
            arr[(rnd.Next(arr.Length-1))]

    let getTargettedNode (network: UndirectedGraph<'NodeKey, 'EdgeData>)   =  
        network
        |> UndirectedGraph.getNodes
        |> Seq.sortByDescending(fun k -> UndirectedGraph.getEdges k network |> Array.length )
        |> Seq.head

    let getRandomNodeFGraph (network: FGraph<'NodeKey, 'NodeData, 'EdgeData>)  =  
        let rnd = System.Random() 
        network
        |> Seq.toArray
        |> fun arr ->
            arr[(rnd.Next(arr.Length-1))].Key
        
    let getTargettedNodeFGraph (network: FGraph<'NodeKey, 'NodeData, 'EdgeData>)   =  
        network
        |> Seq.sortByDescending(fun (KeyValue(k,v)) -> 
            let _,_,s = v 
            s.Count)
        |> Seq.map(fun (KeyValue(k,v)) -> k)
        |> Seq.head
        
    let randomDisruption (network: UndirectedGraph<'NodeKey, 'EdgeData>)  nodesToDisrupt = 
        let rec loop network nodesToDisrupt counter =
            let newCount =  counter + 1
            let trgettedNode = getRandomNode network
            let disruptedNetwork = UndirectedGraph.removeNode trgettedNode network
            if newCount = nodesToDisrupt then disruptedNetwork
            else  loop disruptedNetwork nodesToDisrupt newCount  

        loop network nodesToDisrupt 0

    let targettedDisruption (network: UndirectedGraph<'NodeKey, 'EdgeData>) nodesToDisrupt = 
        let rec loop network nodesToDisrupt counter =
            let newCount =  counter + 1
            let trgettedNode = getTargettedNode network
            let disruptedNetwork = UndirectedGraph.removeNode trgettedNode network
            if newCount = nodesToDisrupt then disruptedNetwork
            else  loop disruptedNetwork nodesToDisrupt newCount  

        loop network nodesToDisrupt 0

    let randomDisruptionFGraph (network: FGraph<'NodeKey, 'NodeData, 'EdgeData>)  nodesToDisrupt = 
        let rec loop network nodesToDisrupt counter =
            let newCount =  counter + 1
            let trgettedNode = getRandomNodeFGraph network
            let disruptedNetwork = FGraph.removeNode trgettedNode network
            if newCount = nodesToDisrupt then disruptedNetwork
            else  loop disruptedNetwork nodesToDisrupt newCount  

        loop network nodesToDisrupt 0

    let targettedDisruptionFGraph (network: FGraph<'NodeKey, 'NodeData, 'EdgeData>) nodesToDisrupt = 
        let rec loop network nodesToDisrupt counter =
            let newCount =  counter + 1
            let trgettedNode = getTargettedNodeFGraph network
            let disruptedNetwork = FGraph.removeNode trgettedNode network
            if newCount = nodesToDisrupt then disruptedNetwork
            else  loop disruptedNetwork nodesToDisrupt newCount  

        loop network nodesToDisrupt 0

module ResilienceChecker = 

    open SupplyNetwork
    open FSharp.Stats
    open FSharp.Stats.Distributions
    open GraphoscopeExtensions
    open Graphoscope
    open Graphoscope.Graph
    open Graphoscope.FGraph

    let linkSupplyandDemand (network: FGraph<'NodeKey,NodeType,float>) 
        (demandNodeSelector: FGraph<'NodeKey,NodeType,float> -> 'NodeKey option) 
        (supplyNodeSelector: FGraph<'NodeKey,NodeType,float> -> 'NodeKey option) = 
            let rec loop network = 
                match (demandNodeSelector network), (supplyNodeSelector network)  with 
                | Some dn, Some sn -> 
                    loop (
                        network
                        |> FGraph.addEdge sn dn (availableCapcity sn dn network)
                            )
                | _ -> network 
        
            loop network

    let getDemandNodeByVolumeAscending (network: FGraph<int,NodeType,float>) = 
        network
        |> Seq.filter(fun (KeyValue(k,v)) -> 
            let _, nd, _ = v
            match nd with | Demand _ -> true | _ -> false
            &&  (getIncomingEdgesWeight k network) < (getWeight k network) )
        |> Seq.sortBy(fun (KeyValue(k,v)) -> (getWeight k network) - (getIncomingEdgesWeight k network) )
        |> Seq.map(fun (KeyValue(k,v)) ->k )
        |> Seq.tryHead 

    let getSupplyNodeByDegree (network: FGraph<int,NodeType,float>) = 
        network   
        |> Seq.filter(fun (KeyValue(k,v)) -> 
            let _, nd, s = v          
            match nd with | Supply _ -> true | _ -> false
            && (getOutgoingEdgesWeight k network) < (getWeight k network) )
        |> Seq.sortByDescending(fun (KeyValue(k,v)) -> 
            FContext.outwardDegree v
            )
        |> Seq.map(fun (KeyValue(k,v)) ->k )
        |> Seq.tryHead

    let getSupplyNodeByVolume network = 
        network
        |> Seq.filter(fun (KeyValue(k,v)) -> 
            let _, nd, s = v                  
            match nd with | Supply _ -> true | _ -> false
            && (getOutgoingEdgesWeight k network) < (getWeight k network) )
        |> Seq.sortByDescending(fun (KeyValue(k,v)) -> (getWeight k network) - (getOutgoingEdgesWeight k network))
        |> Seq.map(fun (KeyValue(k,v)) ->k )
        |> Seq.tryHead


    let getTargettedSupplyNode (network: FGraph<int,NodeType,float>)  =  
        network
        |> Seq.filter(fun (KeyValue(k,v)) -> 
            let _, nd, s = v                  
            match nd with | Supply _ -> true | _ -> false)
        |> Seq.sortByDescending(fun (KeyValue(k,v)) -> FContext.outwardDegree v)
        |> Seq.map(fun (KeyValue(k,v)) -> k )
        |> Seq.head

    let getRandomSupplyNode (network: FGraph<int,NodeType,float>)  =  
        let rnd = System.Random() 
        network
        |> Seq.filter(fun (KeyValue(k,v)) -> 
            let _, nd, s = v                  
            match nd with | Supply _ -> true | _ -> false)
        |> Seq.toArray
        |> fun arr ->
            arr[(rnd.Next(arr.Length-1))].Key

    let targettedDisruption (network: UndirectedGraph<'NodeKey, 'EdgeData>) nodesToDisrupt = 
        let rec loop network nodesToDisrupt counter =
            let newCount =  counter + 1
            let trgettedNode = getTargettedNode network
            let disruptedNetwork = UndirectedGraph.removeNode trgettedNode network
            if newCount = nodesToDisrupt then disruptedNetwork
            else  loop disruptedNetwork nodesToDisrupt newCount  

        loop network nodesToDisrupt 0

    let randomDisruption (network: UndirectedGraph<'NodeKey, 'EdgeData>)  nodesToDisrupt = 
        let rec loop network nodesToDisrupt counter =
            let newCount =  counter + 1
            let trgettedNode = getRandomNode network
            let disruptedNetwork = UndirectedGraph.removeNode trgettedNode network
            if newCount = nodesToDisrupt then disruptedNetwork
            else  loop disruptedNetwork nodesToDisrupt newCount  

        loop network nodesToDisrupt 0

    let randomDisruptionFGraph (network: FGraph<'NodeKey, 'NodeData, 'EdgeData>)  nodesToDisrupt = 
        let rec loop network nodesToDisrupt counter =
            let newCount =  counter + 1
            let trgettedNode = getRandomNodeFGraph network
            let disruptedNetwork = FGraph.removeNode trgettedNode network
            if newCount = nodesToDisrupt then disruptedNetwork
            else  loop disruptedNetwork nodesToDisrupt newCount  

        loop network nodesToDisrupt 0

    let targettedDisruptionFGraph (network: FGraph<'NodeKey, 'NodeData, 'EdgeData>) nodesToDisrupt = 
        let rec loop network nodesToDisrupt counter =
            let newCount =  counter + 1
            let trgettedNode = getTargettedNodeFGraph network
            let disruptedNetwork = FGraph.removeNode trgettedNode network
            if newCount = nodesToDisrupt then disruptedNetwork
            else  loop disruptedNetwork nodesToDisrupt newCount  

        loop network nodesToDisrupt 0

    let getDemandNodes (network: FGraph<'NodeKey,NodeType,'EdgeData>) = 
        network
        |> Seq.filter(fun (KeyValue(k,v)) -> 
            let p, nd, s = v                  
            match nd with | Demand _ -> true | _ -> false)

    let getSupplyNodes (network: FGraph<'NodeKey,NodeType,'EdgeData>) = 
        network
        |> Seq.filter(fun (KeyValue(k,v)) -> 
            let _, nd, s = v                  
            match nd with | Supply _ -> true | _ -> false)

    let getSupplyAvailability 
        (startingdemandCount: float) 
        (network: FGraph<'NodeKey,NodeType,'EdgeData>)  = 
        
        let demandNodes = getDemandNodes network
        let supplyNodes = getSupplyNodes network
        
        let connectdemandNodes = 
            demandNodes
            |> Seq.map(fun (KeyValue(k,v)) -> 
                
                DFSUndirectedFGraph k network
                |> Seq.exists(fun (k,v) -> 
                    match v with | Supply _ -> true | _ -> false)
            )
            |> Seq.filter id
            |> Seq.length
            |> float

        connectdemandNodes / startingdemandCount

    let linkDemandNodesRandomly (proportion: float) (graph: FGraph<'NodeKey,NodeType, float>) = 
        graph
        |> Seq.filter(fun (KeyValue(k,v)) -> 
            let _, nd, _ = v 
            match nd with  
            | Supply _ -> false 
            | Demand _ -> true)
        |> fun x -> 
            let demandNodes = (x |> Seq.length) |> float
                
            Seq.initInfinite (fun i ->  // update this to link high degree demand nodes to low degree demand nodes
                sampleWithOutReplacement rng (graph |> getDemandNodes |> Seq.toArray) 2 
            )
            |> Seq.distinct
            |> Seq.take (int(demandNodes * proportion))
            |> Seq.iter(fun x -> FGraph.addEdge x[0].Key x[1].Key 1.0 graph |> ignore)
        graph




    let linkDemandNodesByAscendingSupplyDegree (proportion: float) (graph: FGraph<'NodeKey, NodeType, float>) =
        let demandNodesWithLowdegreeSupplyNodes = 
            graph
            |> getDemandNodes
            |> Seq.filter(fun (KeyValue(k,v)) -> 
                 let p, _, _ = v
                 if p.Count = 0 then false else true)
            |> Seq.map(fun (KeyValue(k,v)) -> 
                let p, _, _ = v
                k,
                (
                    p |> Seq.minBy(fun (KeyValue(k,_)) -> FContext.outwardDegree graph[k])
                )

            )
            |> Seq.map(fun (dn, sn) -> 
                dn, sn, FContext.outwardDegree graph[sn.Key]
                )
            |> Seq.sortBy(fun (dn,sn,d) -> d)
            |> Seq.toArray

        let len = demandNodesWithLowdegreeSupplyNodes |> Array.length
        let arr = demandNodesWithLowdegreeSupplyNodes

        arr
        |> fun x -> 
            let len = arr |> Array.length
            if len % 2 = 1  
                then arr  |> Array.take (len - 1)
                else arr
        |> Array.splitInto 2
        |> fun x ->  Seq.zip x[0]  x[1]
        |> Seq.iter(fun x -> 
             let demandNode1, _, _ =  snd x
             let demandNode2, _, _ = fst x
             FGraph.addEdge demandNode1 demandNode2 1.0 graph |> ignore
             )
        graph

    let linkDemandNodesBySupplyDegree (proportion: float) (graph: FGraph<'NodeKey, NodeType, float>) = 
        graph
        |> Seq.filter(fun (KeyValue(k,v)) -> 
            let _, nd, _ = v 
            match nd with  
            | Supply _ -> false 
            | Demand _ -> true)
        |> fun x -> 
            let demandNodes = (x |> Seq.length) |> float

            let bestConnectedDemandNodes = 
                graph 
                |> getDemandNodes
                |> Seq.sortByDescending(fun (KeyValue(k,v)) -> 
                    let p,_,s = v
                    p.Count
                )
                |>  Seq.take 5
                |> Seq.toArray
                
            Seq.initInfinite (fun i ->  // update this to link high degree demand nodes to low degree demand nodes
                ((sampleWithOutReplacement rng (graph |> getDemandNodes |> Seq.toArray) 1)|> Array.head).Key, bestConnectedDemandNodes[rng.Next(5)].Key
            )
            |> Seq.distinct
            |> Seq.filter(fun (f,t) -> f<>t) 
            |> Seq.take (int(demandNodes * proportion))
            |> Seq.iter(fun (f,t) -> FGraph.addEdge f t 1.0 graph |> ignore)
        graph

    type LinkDriver = Volume | Degree 

    let createSupplyNetwork 
        (demandNodes: int) 
        (supplyNodes: int) 
        (supplyCapacityDistribution: ContinuousDistribution<float, float>) 
        (demandCapacityDistribution: ContinuousDistribution<float, float>)  
        (linkDriver: LinkDriver) =  

        let nodes = 
            List.init demandNodes (fun _ -> Demand (demandCapacityDistribution.Sample()))
            |> List.append (List.init supplyNodes (fun _ -> Supply (supplyCapacityDistribution.Sample())))
            |> List.mapi(fun i n -> i, n)

        let g = 
            FGraph.empty
            |> FGraph.addNodes nodes

        match linkDriver with 
        | Degree -> linkSupplyandDemand g getDemandNodeByVolumeAscending getSupplyNodeByDegree
        | Volume -> linkSupplyandDemand g getDemandNodeByVolumeAscending getSupplyNodeByVolume

module DataVisualisation = 
    open SupplyNetwork
    open GraphoscopeExtensions
    open Graphoscope.Graph
    open Graphoscope
    open FGraph
    open Cytoscape.NET
    open Plotly.NET
    open Plotly.NET.LayoutObjects
    open HnCTemplate
    open Feliz
    open System.Diagnostics
    open System.IO
    open System
    open ResilienceChecker

    let showDegreeDistribution name graph = 
        graph
        |> getDegreeDistribution
        |> Chart.Histogram
        |> Chart.withTitle (sprintf "Degree Distribution for a %s graph" name)
        |> Chart.withXAxisStyle ("Degree")
        |> Chart.withYAxisStyle ("Node count")
        |> Chart.withTemplate Light.hncTemplate

    let showDegreeDistributionLogLog (title: string) (network: UndirectedGraph<'NodeKey','EdgeData>)  =
        let degreeXAxis =
            LinearAxis.init (
                Title = Title.init (Text = "Degree axiss"),
                ShowLine = false,
                AxisType = StyleParam.AxisType.Log

            )
        let frequencyYAxis =
            LinearAxis.init (
                Title = Title.init (Text = "Count axis"),
                ShowLine = false,
                AxisType = StyleParam.AxisType.Log
                
            )
        let data = 
            network
            |> UndirectedGraph.getNodes
            |> Array.map(fun k  -> 
                UndirectedGraph.getEdges k network |> Array.length |> float
                )
            |> Array.countBy id
            |> Array.sortBy fst

        Chart.Scatter (xy = data, Name = title,  mode = StyleParam.Mode.Markers )
        |> Chart.withXAxis degreeXAxis
        |> Chart.withYAxis frequencyYAxis
        |> Chart.withTemplate Light.hncTemplate


    let getDisruptionGraphByLargestCompoenent 
        (graph: UndirectedGraph<'NodeKey, 'EdgeData>) 
        (disruptor: UndirectedGraph<'NodeKey,'EdgeData> -> int ->  UndirectedGraph<'NodeKey, 'EdgeData>) 
        title 
        step = 
        let nodeCount = (UndirectedGraph.getNodes graph |> Array.length)
        let disruptions = [1..step..( nodeCount- 1)]
        let largestComponent = 
            disruptions
            |> List.map(fun i -> 
                printfn "Disruptions : %i" i
                (disruptor graph step) |> getLargestComponentSize)

        Chart.Line(x = disruptions, y = largestComponent,  Name = title )
        |> Chart.withTemplate Light.hncTemplate

    let getDisruptionGraphByLargestCompoenentFGraph 
        (graph: FGraph<'NodeKey, 'NodeData, 'EdgeData>) 
        (disruptor: FGraph<'NodeKey, 'NodeData, 'EdgeData> -> int ->  FGraph<'NodeKey, 'NodeData, 'EdgeData>) 
        title 
        step
        disruptions = 
        let nodeCount = FGraph.countNodes graph
        let disruptions = [1..step..disruptions]
        let largestComponent = 
            disruptions
            |> List.map(fun i -> 
                printfn "Disruptions : %i" i
                (disruptor graph step) |> getLargestComponentSizeFGraph)

        Chart.Line(x = disruptions, y = largestComponent,  Name = title )
        |> Chart.withTemplate Light.hncTemplate

    let displayCharts (charts: GenericChart.GenericChart seq) = 

        let build charts = 
            ViewEngine.Html.html [
                ViewEngine.Html.head [ ViewEngine.Html.title "Charts" ]
                ViewEngine.Html.body [
                    for c in (charts|> Seq.rev) do 
                        ViewEngine.prop.dangerouslySetInnerHTML (c |> Chart.withTemplate Light.hncTemplate |> GenericChart.toEmbeddedHTML)
                    ]
                ]
        
        let document =  ViewEngine.Render.htmlDocument (build charts)
        let fileName = sprintf "%s%s%s" (Path.GetTempPath()) (Guid.NewGuid().ToString()) ".html"
        File.WriteAllText(fileName, document)
        let  psi = new ProcessStartInfo(UseShellExecute = true,  FileName = fileName)
        let p = new Process(StartInfo = psi)
        p.Start()

    let vizGraph graphType (network: FGraph<'NodeKey', 'NodeData', 'EdgeData>) =
        let nodes = 
            network
            |> Seq.map(fun (KeyValue(k,_))  -> 
                Elements.node (string k)  [CyParam.label (k); (CyParam.color "blue"); CyParam.shape "circle"] )

        let edges = 
            network
            |> Seq.map(fun (KeyValue(k,v)) ->
                let _, _, s = v
                s
                |> Seq.map(fun (KeyValue(t,w)) ->  k, t, w)
            )
            |> Seq.concat
            |> Seq.map(fun (s,t,w) -> 
                Elements.edge  (sprintf "%s_%s" (string s) (string t)) 
                    (string s) (string t)
                    [ CyParam.label w ])

        CyGraph.initEmpty ()
        |> CyGraph.withElements edges
        |> CyGraph.withElements nodes
        |> CyGraph.withStyle "background"
            [
                CyParam.Background.color =. (fun x -> (CyParam.color "black"))
            ]
        |> CyGraph.withStyle "node"     
            [   CyParam.content =. CyParam.label
                CyParam.Text.Outline.color =. CyParam.color
                CyParam.Background.color =. CyParam.color
                CyParam.shape =. CyParam.shape  ]
            |> CyGraph.withLayout (if graphType = "circle" then (Layout.initCircle (Layout.LayoutOptions.Generic())) else  (Layout.initCose (Layout.LayoutOptions.Cose(ComponentSpacing = 40))))
            |> CyGraph.show

    let vizUndirectedGraph graphType (weightNodes: bool)  (network: UndirectedGraph<'NodeKey','EdgeData>) =
        let nodes = 
            network
            |> UndirectedGraph.getNodes
            |> Array.map(fun k  -> 
                    let nodeSize = (UndirectedGraph.getEdges k network |> Array.length |> float)
                    Elements.node (string k)  [CyParam.label (k); (CyParam.color "blue"); (CyParam.shape "circle"); if weightNodes then (CyParam.weight (nodeSize * nodeSize))] )

        let edges = 
            network
            |> UndirectedGraph.getAllEdges
            |> Seq.map(fun (f,t,w) ->
                Elements.edge  (sprintf "%s_%s" (string f) (string t)) 
                    (string f) (string t)
                    [ CyParam.label w ])

        CyGraph.initEmpty ()
        |> CyGraph.withElements edges
        |> CyGraph.withElements nodes
        |> CyGraph.withStyle "background"
            [
                CyParam.Background.color =. (fun x -> (CyParam.color "black"))
            ]
        |> CyGraph.withStyle "node"     
            [   CyParam.content =. CyParam.label
                CyParam.Text.Outline.color =. CyParam.color
                CyParam.Background.color =. CyParam.color
                if weightNodes then 
                    CyParam.height =. CyParam.weight
                    CyParam.width =. CyParam.weight
                CyParam.shape =. CyParam.shape  ]
            |> CyGraph.withLayout (if graphType = "circle" then (Layout.initCircle (Layout.LayoutOptions.Generic())) else  (Layout.initCose (Layout.LayoutOptions.Cose(NodeOverlap = 400,ComponentSpacing = 100))))
            |> CyGraph.withSize(1200,800)
            |> CyGraph.show



    let vizSupplyGraph (network: FGraph<int,NodeType,float>) =

        let setNodeColour nodeId (network: FGraph<int,NodeType,float>) =
            let _, nd, s = network[nodeId]  
            match nd with 
            | Supply c -> 
                if (getOutgoingEdgesWeight nodeId network) < c then CyParam.color "blue";
                    else CyParam.color "black";
            | Demand v -> 
                if (getIncomingEdgesWeight nodeId network) >= v then CyParam.color "green";
                    else CyParam.color "red";
            
        let nodes = 
            network
                |> Seq.map(fun (KeyValue(k,v))  -> 
                    let p, nd, s= v

                    let nodeSize = 
                        match nd with 
                        | Demand _ -> p.Count
                        | Supply _ -> s.Count

                    Elements.node (string k)
                        (match nd with 
                        | Supply _ -> [CyParam.label (k); (setNodeColour k network); CyParam.shape "square"; (CyParam.weight nodeSize)] 
                        | Demand _ -> [CyParam.label (k);  (setNodeColour k network); CyParam.shape "triangle"; (CyParam.weight (nodeSize * 10))] ) )

        let edges = 
            network
            |> Seq.map(fun (KeyValue(k,v)) ->
                let _, _, s = v
                s
                |> Seq.map(fun (KeyValue(t,w)) ->  k, t, w)
            )
            |> Seq.concat
            |> Seq.map(fun (s,t,w) -> 
                Elements.edge  (sprintf "%s_%s" (string s) (string t)) 
                    (string s) (string t)
                    [ CyParam.label w ])

        CyGraph.initEmpty ()
        |> CyGraph.withElements edges
        |> CyGraph.withElements nodes
        |> CyGraph.withStyle "background"
            [
                CyParam.Background.color =. (fun x -> (CyParam.color "black"))
            ]
        |> CyGraph.withStyle "node"     
            [   CyParam.content =. CyParam.label
                CyParam.Text.Outline.color =. CyParam.color
                CyParam.Background.color =. CyParam.color
                CyParam.height =. CyParam.weight
                CyParam.width =. CyParam.weight
                CyParam.shape =. CyParam.shape  ]
            |> CyGraph.withLayout (Layout.initCose (Layout.LayoutOptions.Cose(ComponentSpacing = 40)))
            |> CyGraph.show

    let compareResilience 
        (builder : unit -> FGraph<'NodeKey, NodeType, 'EdgeData>) 
        (disrupter:FGraph<'NodeKey, NodeType, 'EdgeData> -> int -> FGraph<'NodeKey, NodeType, 'EdgeData>) 
        (name: string) = 
        
        let g = builder()
        
        let demandNodes = 
            g 
            |> Seq.filter(fun (KeyValue(k,v)) -> 
                let _, nd, s = v                  
                match nd with | Demand _ -> true | _ -> false)
            |> Seq.length

        let xAxis = [1..1..50]
        let yAxis = 
            xAxis
            |> List.map(fun i -> (disrupter g  1) |> getSupplyAvailability demandNodes)
        
        Chart.Line(x =xAxis, y =yAxis,  Name = name)

////DEMO//////
open Plotly.NET
open FSharp.Stats
open Graphoscope.Graph
open GraphGenerators
open DataVisualisation
open HnCTemplate.Light
open ResilienceChecker
open GraphoscopeExtensions

(*
//////////VISUALISE GRAPH TYPES//////////////////////////
// Complete Graph - shortest path of 1 
createCompleteGraph 16 
|> vizUndirectedGraph "circle" false

createRing 16 4
|> vizUndirectedGraph "circle" false

createCompleteGraph 16 
|> vizUndirectedGraph "circle" false

// Random Graph - with no SW or Hubs
(createGilbert 16 0.2) 
|> vizUndirectedGraph "" false

// ErdosRenyi with small world
(createErdosRenyi 50 122)
|> vizUndirectedGraph "" true

// Barabasi short shortest paths and Hubs
(createBarabasiAlbert 2 48)
|> getGiantComponent
|> vizUndirectedGraph "" true

//////////SHOW COMPARABLE NODE AND EDGE COUNTS//////////////////////////
let compareCounts = 
    [
    //"Complete", (createCompleteGraph 1000 )
    "Gilbert", (createGilbert 100 0.0398) ;
    "Erdos Renyi", (createErdosRenyi 100 197);
    "Barabasci-Albert", (createBarabasiAlbert 2 98)
    ]
    |> List.map(fun (n, g) -> n , UndirectedGraph.getAllEdges g |> Array.length,  UndirectedGraph.getNodes g |> Array.length)
    |> List.map(fun (t, e, n) -> [e, (sprintf "%s edges" t) ;  n, (sprintf "%s nodes" t)]  )
    |> List.concat
    |> List.unzip

Chart.Column(values = (fst compareCounts), Keys = (snd compareCounts))
|> Chart.withTemplate hncTemplate
|> Chart.show

//////////CONTRASTING DEGREE DISTRIBUTION///////////////////////////

// Histograms
[
//"Complete", (createCompleteGraph 1000 )
"Gilbert", (createGilbert 1000 0.0398) ;
"Erdos Renyi", (createErdosRenyi 1000 1970);
"Barabasci-Albert", (createBarabasiAlbert 2 980)
]
|> dict
|> Seq.map(fun (KeyValue(k,v)) ->  showDegreeDistribution k v)
|> displayCharts

// Log - Log plots
[
//"Complete", (createCompleteGraph 1000 )
//"Gilbert", (createGilbert 1000 0.0398) ;
"Erdos Renyi", (createErdosRenyi 1000 1970);
"Barabasci-Albert", (createBarabasiAlbert 2 980)
]
|> dict
|> Seq.map(fun (KeyValue(k,v)) ->  showDegreeDistributionLogLog k v)
|> Chart.combine
|> Chart.show

//// COMPARE SHORTEST PATHS /////

let erPathLengths = 
    [|10..10..1000|] 
    |> Array.map (fun i -> 
        createErdosRenyi i (i*2)
        |> getGiantComponent
        |> dijkstraComputeAllPairs
        |> averagePathLength
    )

let baPathLengths = 
    [|10..10..1000|] 
    |> Array.map (fun i -> 
        createBarabasiAlbert 2 i
        |> getGiantComponent
        |> dijkstraComputeAllPairs
        |> averagePathLength
    )

let rlPathLengths = 
    [|10..10..1000|] 
    |> Array.map (fun i -> 
        createRing i 2
        |> getGiantComponent
        |> dijkstraComputeAllPairs
        |> averagePathLength
    )


let xAxis = [|10..10..1000|] 
[
Chart.Line(x = xAxis , y = erPathLengths, Name = "Erdos Renyi")
Chart.Line(x = xAxis , y = baPathLengths , Name = "Barabasi-Albert")
Chart.Line(x = xAxis , y = rlPathLengths , Name = "Regular Ring Lattice")
]
|> Chart.combine
|> Chart.withTitle "Average Shortest Path for different models"
|> Chart.withXAxisStyle ("Nodes")
|> Chart.withYAxisStyle ("Avg Shortest Path")
|> Chart.withTemplate hncTemplate
|> Chart.show


//////////RESILIENCE BY SIZE OF LARGEST COMPONENT////////////////////
let t = 
    [|
    //"Complete", (createCompleteGraph 1000 )
    "Gilbert", (createGilbert 100 0.05);
    "Erdos Renyi", (createErdosRenyi 100 201);
    "Barabasci-Albert", (createBarabasiAlbert 2 98)
    |]
    |> Array.Parallel.map(fun (k,v) -> getDisruptionGraphByLargestCompoenent v (targettedDisruption) (sprintf "Targetted Attack - %s" k) 1)
    |> Chart.combine

let r =
    [|
    //"Complete", (createCompleteGraph 1000 )
    "Gilbert", (createGilbert 100 0.05);
    "Erdos Renyi", (createErdosRenyi 100 201);
    "Barabasci-Albert", (createBarabasiAlbert 2 98)
    |]
    |> Seq.toArray
    |> Array.Parallel.map(fun (k,v) ->  getDisruptionGraphByLargestCompoenent v randomDisruption (sprintf "Random Attack - %s" k)  1)
    |> Chart.combine

[
    r
    t
]
|> Chart.combine
|> Chart.withSize(1200, 800)
|> Chart.withTitle "Largest compoenet size under increaseing disruption"
|> Chart.withXAxisStyle ("Disrupted nodes")
|> Chart.withYAxisStyle ("Size of largest component")
|> fun x -> [x]
|> displayCharts


*)
//////////HETEROGENOUS DIRECTED SUPPLY NETWORKS //////////////////

//////////VISUALISE FRAGMENTED SUPPLY CHAIN//////////////////
let demandDist = Distributions.Continuous.Normal.Init 5. 2.
let supplyDist = Distributions.Continuous.Normal.Init 100. 5.

let supplyNetwork() = createSupplyNetwork 500 24 supplyDist demandDist Volume  // single 

supplyNetwork()  |> vizSupplyGraph

/////TEST RESILIENCE OF SUPPLY CHAIN BY LARGEST COMPONENT/////
[
getDisruptionGraphByLargestCompoenentFGraph (supplyNetwork()) randomDisruptionFGraph "Random attack" 1 100
getDisruptionGraphByLargestCompoenentFGraph (supplyNetwork()) targettedDisruptionFGraph "Targetted attack" 1 100
]
|> Chart.combine
|> Chart.show

///// INTRODUCE SUPPLY AVAILABILITY AS RESILIENCE METRIC ///////
let buildNetwork() = createSupplyNetwork 500 50 supplyDist demandDist Volume
[
compareResilience buildNetwork randomDisruptionFGraph "Random attack" 
compareResilience buildNetwork targettedDisruptionFGraph "Targetted attack" 
]
|> Chart.combine
|> Chart.withTemplate hncTemplate
|> Chart.show


let buildSupplyDegreeLinkedSC() = 
    buildNetwork() 
    |> linkDemandNodesBySupplyDegree 0.25

let buildRandomlyLinkedSC() = 
    buildNetwork() 
    |> linkDemandNodesRandomly 0.25

let buildByAscendingSupplyDegree() = 
    buildNetwork() 
    |> linkDemandNodesByAscendingSupplyDegree 0.25

[
compareResilience buildNetwork targettedDisruptionFGraph "Targetted attack" 
//compareResilience buildNetwork randomDisruptionFGraph "Random attack" 
compareResilience buildRandomlyLinkedSC targettedDisruptionFGraph "Targetted attack on randomly linked network" 
//compareResilience buildRandomlyLinkedSC randomDisruptionFGraph "Random attack on randomly linked network" 
compareResilience buildSupplyDegreeLinkedSC targettedDisruptionFGraph "Targetted attack on degree linked network" 
//compareResilience buildSupplyDegreeLinkedSC randomDisruptionFGraph "Random attack on degree linked network" 
compareResilience buildByAscendingSupplyDegree targettedDisruptionFGraph "Targetted attack on low supply degree linked network" 
//compareResilience buildByAscendingSupplyDegree randomDisruptionFGraph "Random attack on low supply degree linked network" 
]
|> Chart.combine
|> Chart.withSize(1200, 800)
|> Chart.withTemplate hncTemplate
|> Chart.show


