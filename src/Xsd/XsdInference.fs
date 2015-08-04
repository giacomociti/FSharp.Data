namespace ProviderImplementation

open System.Xml
open System.Xml.Schema

module XsdModel =

    type XsdElement = { Name: XmlQualifiedName; Type: XsdType; IsNillable: bool }

    and XsdType = SimpleType of XmlTypeCode | ComplexType of XsdComplexType

    and XsdComplexType = 
        { Attributes: (XmlQualifiedName * XmlTypeCode * bool) list // bool is for optional
          Contents: XsdContent 
          IsMixed: bool }

    and XsdContent = SimpleContent of XmlTypeCode | ComplexContent of XsdParticle

    and Occurs = decimal * decimal

    and XsdParticle = 
        | Empty
        | Any      of Occurs 
        | Element  of Occurs * XsdElement
        | All      of Occurs * XsdParticle list
        | Choice   of Occurs * XsdParticle list
        | Sequence of Occurs * XsdParticle list

module XsdParsing =

    type ResolutionFolderResolver(resolutionFolder) =
        inherit XmlUrlResolver()
        override _this.ResolveUri(_, relativeUri) = 
            System.Uri(System.IO.Path.Combine(resolutionFolder, relativeUri))

    
    open XsdModel    
    open Microsoft.FSharp.Core

    let inline ofType<'a> sequence = System.Linq.Enumerable.OfType<'a> sequence

    let hasCycles x = 
        let items = System.Collections.Generic.HashSet<XmlSchemaObject>()
        let rec closure (obj: XmlSchemaObject) =
            let nav innerObj =
                if items.Add innerObj then closure innerObj
            match obj with
            | :? XmlSchemaElement as e -> 
                nav e.ElementSchemaType 
            | :? XmlSchemaComplexType as c -> 
                nav c.ContentTypeParticle
            | :? XmlSchemaGroupRef as r -> 
                nav r.Particle
            | :? XmlSchemaGroupBase as x -> 
                x.Items 
                |> ofType<XmlSchemaObject> 
                |> Seq.iter nav
            | _ -> ()
        closure x
        items.Contains x


    let rec parseElement (elm: XmlSchemaElement) =  
        
        if hasCycles elm then failwith "Recursive schemas are not supported yet."

        let rec parseParticle (par: XmlSchemaParticle) =

            let occurs = par.MinOccurs, par.MaxOccurs

            let parseParticles (group: XmlSchemaGroupBase) =
                let particles = 
                    group.Items
                    |> ofType<XmlSchemaParticle> 
                    |> Seq.map parseParticle
                    |> List.ofSeq // beware of recursive schemas
                match group with
                | :? XmlSchemaAll      -> All (occurs, particles)
                | :? XmlSchemaChoice   -> Choice (occurs, particles)
                | :? XmlSchemaSequence -> Sequence (occurs, particles)
                | _ -> failwithf "unknown group base: %A" group

            match par with
            | :? XmlSchemaAny -> Any occurs
            | :? XmlSchemaGroupBase as grp -> parseParticles grp
            | :? XmlSchemaGroupRef as grpRef -> parseParticle grpRef.Particle
            | :? XmlSchemaElement as elm -> Element (occurs, parseElement elm)
            | _ -> Empty // XmlSchemaParticle.EmptyParticle

        { Name = elm.QualifiedName
          Type = 
            match elm.ElementSchemaType with
            | :? XmlSchemaSimpleType  as x -> SimpleType x.Datatype.TypeCode
            | :? XmlSchemaComplexType as x -> 
                ComplexType
                    { Attributes = 
                        x.AttributeUses.Values |> ofType<XmlSchemaAttribute>
                        |> Seq.filter (fun a -> a.Use <> XmlSchemaUse.Prohibited)
                        |> Seq.map (fun a -> a.QualifiedName,
                                             a.AttributeSchemaType.Datatype.TypeCode, 
                                             a.Use <> XmlSchemaUse.Required)
                        |> List.ofSeq
                      Contents = 
                          match x.ContentType with
                            | XmlSchemaContentType.TextOnly -> SimpleContent x.Datatype.TypeCode
                            | XmlSchemaContentType.Mixed 
                            | XmlSchemaContentType.Empty 
                            | XmlSchemaContentType.ElementOnly -> 
                                x.ContentTypeParticle |> parseParticle |> ComplexContent
                            | _ -> failwith "Unknown content type: %A." x.ContentType

                      IsMixed = x.IsMixed }
            | x -> failwithf "unknown ElementSchemaType: %A" x
          IsNillable = elm.IsNillable }


    let parseSchema resolutionFolder xsdText =
        let schemaSet = new XmlSchemaSet()
        if resolutionFolder <> "" then
            schemaSet.XmlResolver <- ResolutionFolderResolver(resolutionFolder)
        use reader = new XmlTextReader(new System.IO.StringReader(xsdText))
        XmlSchema.Read(reader, null) |> schemaSet.Add |> ignore
        schemaSet.Compile()
        schemaSet


    let getElement elmName elmNs (schema: XmlSchemaSet) =
        schema.GlobalElements.Values 
        |> ofType<XmlSchemaElement>
        |> Seq.tryFind (fun x -> 
            x.QualifiedName.Name = elmName && 
            x.QualifiedName.Namespace = elmNs)
        |> Option.map parseElement
        |> function 
            | None -> failwithf "No element found with name '%s' and namespace '%s'." elmName elmNs
            | Some e -> e



module XsdInference =
    open XsdModel
    open FSharp.Data.Runtime.StructuralTypes

    let mult = function
        | 1M, 1M -> InferedMultiplicity.Single
        | 0M, 1M -> InferedMultiplicity.OptionalSingle
        | _ -> InferedMultiplicity.Multiple

    let getType = function
        | XmlTypeCode.Int -> typeof<int>
        | XmlTypeCode.Date -> typeof<System.DateTime>
        | XmlTypeCode.Boolean -> typeof<bool>
        | XmlTypeCode.Decimal -> typeof<decimal>
        // etc..
        | _ -> typeof<string>

    let rec inferElement elm =
        
        let rec inferParticle = function
            | Element (occurs, e) -> 
                InferedTypeTag.Record(Some e.Name.Name), 
                (mult occurs, inferElement e)
            | All (occurs, items) 
            | Sequence (occurs, items) -> 
                let inner = items |> List.map inferParticle
                let tags  = inner |> List.map fst
                let types = inner |> Map.ofList
                InferedTypeTag.Collection, 
                (mult occurs, InferedType.Collection(tags, types))
            | Choice (occurs, items) ->  
                let makeOptional = function Single -> OptionalSingle | x -> x
                let inner = items |> List.map inferParticle
                let tags  = inner |> List.map fst
                let types = 
                    inner 
                    |> List.map (fun (tag, (mult, ty)) -> tag, (makeOptional mult, ty))
                    |> Map.ofList
                InferedTypeTag.Collection, 
                (mult occurs, InferedType.Collection(tags, types))
            | Empty -> 
                InferedTypeTag.Collection,
                (InferedMultiplicity.OptionalSingle, InferedType.Top)
            | Any _ -> 
                let name = Some "{anyNs}anyElement"
                InferedTypeTag.Record(name), 
                (InferedMultiplicity.Multiple, 
                 InferedType.Record (name, [{Name = ""; Type = InferedType.Top}], false))
                

        let name = Some elm.Name.Name
        match elm.Type with
        | SimpleType typeCode ->
            let body: InferedProperty =
                { Name = ""
                  Type = InferedType.Primitive (getType typeCode, None, elm.IsNillable) }
            InferedType.Record(name, [body], optional = false)
                  
        | ComplexType c -> 
            let attrs: InferedProperty list = 
                c.Attributes
                |> List.map (fun (name, typeCode, optional) ->
                    { Name = name.Name
                      Type = InferedType.Primitive (getType typeCode, None, optional) } )

            match c.Contents with
            | SimpleContent typeCode -> 
                let body: InferedProperty = 
                    { Name = ""
                      Type = InferedType.Primitive (getType typeCode, None, elm.IsNillable) }
                InferedType.Record(name, body::attrs, optional = false)

            | ComplexContent xsdParticle ->
                let _,(_, ty) = inferParticle xsdParticle
                let body: InferedProperty = { Name = ""; Type = ty }
                InferedType.Record(name, body::attrs, optional = false)
