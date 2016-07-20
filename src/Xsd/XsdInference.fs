﻿// --------------------------------------------------------------------------------------
// Implements XML type inference from XSD
// --------------------------------------------------------------------------------------

// The XML Provider infers a type from sample documents. An instance of InferedType 
// represents a class of elements having a structure compatible with the given samples.
// When a schema is availbale we can use it to derive an InferedType representing
// valid elements according to an element definition in the given schema.
// The InferedType derived from a schema should be essentialy the same as one
// infered from a significant set of valid samples.
// This is an easy way to support some XSD leveraging the existing functionalities.
// The implementation uses a simplfied XSD model to split the task of deriving an InferedType:
// - element definitions in xsd files map to this simplified xsd model
// - instances of this xsd model map to InferedType.




namespace ProviderImplementation

open System.Xml
open System.Xml.Schema

/// Simplified model to represent schemas (XSD).
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

/// A simplified schema model is built from xsd. 
/// The actual parsing is done using BCL classes.
module XsdParsing =

    /// A custom XmlResolver is needed for included files because we get the contents of the main file 
    /// directly as a string from the FSharp.Data infrastructure. Hence the default XmlResolver is not
    /// able to find the location of included schema files.
    type ResolutionFolderResolver(resolutionFolder) =
        inherit XmlUrlResolver()
        override _this.ResolveUri(_, relativeUri) = 
            System.Uri(System.IO.Path.Combine(resolutionFolder, relativeUri))

    
    open XsdModel

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


        { Name = elm.QualifiedName
          Type = 
            match elm.ElementSchemaType with
            | :? XmlSchemaSimpleType  as x -> SimpleType x.Datatype.TypeCode
            | :? XmlSchemaComplexType as x -> ComplexType (parseComplexType x)
            | x -> failwithf "unknown ElementSchemaType: %A" x
          IsNillable = elm.IsNillable }

    and parseComplexType (x: XmlSchemaComplexType) =
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
                | _ -> failwithf "Unknown content type: %A." x.ContentType

          IsMixed = x.IsMixed }


    and parseParticle (par: XmlSchemaParticle) =

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



    let parseSchema resolutionFolder xsdText =
        let schemaSet = XmlSchemaSet()
        if resolutionFolder <> "" then
            schemaSet.XmlResolver <- ResolutionFolderResolver(resolutionFolder)
        use reader = new XmlTextReader(new System.IO.StringReader(xsdText))
        XmlSchema.Read(reader, null) |> schemaSet.Add |> ignore
        schemaSet.Compile()
        schemaSet


    let getElement elmName elmNs (schema: XmlSchemaSet) =
        let elms = schema.GlobalElements.Values |> ofType<XmlSchemaElement>
        let names = elms |> Seq.map (fun x -> x.QualifiedName) |> List.ofSeq
        match elmName, Seq.length elms with
        | _ , 0 -> failwith "There are no global elements in the schema"
        | "", 1 -> parseElement (Seq.exactlyOne elms)
        | "", _ -> 
            //parseElement (Seq.head elms)
            failwithf "Element name must be specified because there are multiple global elements: %A." names
        | _ ->
            elms
            |> Seq.tryFind (fun x -> 
                x.QualifiedName.Name = elmName && 
                x.QualifiedName.Namespace = elmNs)
            |> Option.map parseElement
            |> function 
                | None -> failwithf "No element found with name '%s' and namespace '%s'. Available elements: %A." elmName elmNs names
                | Some e -> e


/// Element definitions in a schema are mapped to InferedType instances
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
        | XmlTypeCode.DateTime -> typeof<System.DateTime>
        | XmlTypeCode.Boolean -> typeof<bool>
        | XmlTypeCode.Decimal -> typeof<decimal>
        | XmlTypeCode.Double -> typeof<double>
        | XmlTypeCode.Float -> typeof<single>
        // fallback to string
        | _ -> typeof<string>

    let rec inferElement (elm: XsdElement) =
        
        let name = Some elm.Name.Name
        match elm.Type with
        | SimpleType typeCode ->
            let ty = InferedType.Primitive (getType typeCode, None, elm.IsNillable)
            InferedType.Record(name, [{ Name = ""; Type = ty }], optional = false)
        | ComplexType cty -> 
            InferedType.Record(name, inferComplexType cty, optional = false)


    and inferComplexType cty =
        let attrs: InferedProperty list = 
            cty.Attributes
            |> List.map (fun (name, typeCode, optional) ->
                { Name = name.Name
                  Type = InferedType.Primitive (getType typeCode, None, optional) } )

        match cty.Contents with
        | SimpleContent typeCode -> 
            let ty = InferedType.Primitive (getType typeCode, None, false)
            { Name = ""; Type = ty }::attrs
        | ComplexContent xsdParticle ->
            let _,(_, ty) = inferParticle xsdParticle
            { Name = ""; Type = ty }::attrs


    and inferParticle = function
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




////////////////////////////////////////////////////////////

    open XsdParsing

    let rec inferElement' (elm: XmlSchemaElement) =
        let elm' = parseElement elm
        inferElement elm'

    and inferComplexType' (cty: XmlSchemaComplexType) =
        let cty' = parseComplexType cty
        inferComplexType cty'

    and inferParticle' (par: XmlSchemaParticle) = 
        let par' = parseParticle par
        inferParticle par'
