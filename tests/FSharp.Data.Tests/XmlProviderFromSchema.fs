#if INTERACTIVE
#r "../../bin/FSharp.Data.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#r "System.Xml.Linq.dll"
#load "../Common/FsUnit.fs"
#else
module FSharp.Data.Tests.XmlProviderFromSchema
#endif

open NUnit.Framework
open FSharp.Data
open FsUnit
open System.Xml.Linq

type stringElement = XmlProviderFromSchema<"""
  <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" 
    elementFormDefault="qualified" attributeFormDefault="unqualified">
      <xs:element name="foo" type="xs:string" />
  </xs:schema>""", ElementName = "foo">

[<Test>]
let ``element with simple content``() =
    let foo = stringElement.Parse("""<foo>hello</foo>""")
    foo |> should equal "hello"

    
type elemWithAttrs = XmlProviderFromSchema<"""
  <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" 
    elementFormDefault="qualified" attributeFormDefault="unqualified">
    <xs:element name="foo">
	   <xs:complexType>
		 <xs:attribute name="bar" type="xs:string" use="required" />
         <xs:attribute name="baz" type="xs:int" />
	  </xs:complexType>
	</xs:element>
  </xs:schema>""", ElementName = "foo">

[<Test>]
let ``element with attributes``() =
    let foo = elemWithAttrs.Parse("""<foo bar="hello" />""")
    foo.Bar |> should equal "hello"
    foo.Baz |> should equal None
    let foo = elemWithAttrs.Parse("""<foo bar="hello" baz="2" />""")
    foo.Bar |> should equal "hello"
    foo.Baz |> should equal (Some 2)

type attrsAndSimpleContent = XmlProviderFromSchema<"""
  <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" 
    elementFormDefault="qualified" attributeFormDefault="unqualified">
      <xs:element name="foo">
		<xs:complexType>
		  <xs:simpleContent>
			  <xs:extension base="xs:date">
				  <xs:attribute name="bar" type="xs:string" use="required"/>
				  <xs:attribute name="baz" type="xs:int"/>
			  </xs:extension>
		  </xs:simpleContent>
		</xs:complexType>
	  </xs:element>
  </xs:schema>""", ElementName = "foo">

[<Test>]
let ``element with attributes and simple content``() =
    let date = System.DateTime(1957, 8, 13)
    let foo = attrsAndSimpleContent.Parse("""<foo bar="hello">1957-08-13</foo>""")
    foo.Value |> should equal date
    foo.Bar |> should equal "hello"
    foo.Baz |> should equal None
    let foo = attrsAndSimpleContent.Parse("""<foo bar="hello" baz="2">1957-08-13</foo>""")
    foo.Value |> should equal date
    foo.Bar |> should equal "hello"
    foo.Baz |> should equal (Some 2)


type untypedElement = XmlProviderFromSchema<"""
  <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" 
    elementFormDefault="qualified" attributeFormDefault="unqualified">
      <xs:element name="foo" />
  </xs:schema>""", ElementName = "foo">

[<Test>]
let ``untyped element may contain anything``() =
  let foo = untypedElement.Parse("""
  <foo>
    <anything />
    <greetings>hi</greetings>
  </foo>""")
  foo.AnyElements
  |> Array.iter (printfn "%A")
  // not working as expected
  //foo.AnyElements.[0].XElement.Name.LocalName |> should equal "anything"
  //foo.AnyElements.[1].XElement.Name.LocalName |> should equal "greetings"

  
type elmWithChildSequence = XmlProviderFromSchema<"""
  <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" 
    elementFormDefault="qualified" attributeFormDefault="unqualified">
      <xs:element name="foo">
		<xs:complexType>
			<xs:sequence>
				<xs:element name="bar" type="xs:int"/>
				<xs:element name="baz" type="xs:date" minOccurs="0" maxOccurs="unbounded"/>
			</xs:sequence>
		</xs:complexType>
	</xs:element>
  </xs:schema>""", ElementName = "foo">

[<Test>]
let ``element with child sequence``() =
  let foo = elmWithChildSequence.Parse("""
  <foo>
    <bar>5</bar>
    <baz>1957-08-13</baz>
    <baz>1957-08-14</baz>
  </foo>""")
  foo.Bar |> should equal 5
  foo.Bazs.[0] |> should equal (System.DateTime(1957, 8, 13))
  foo.Bazs.[1] |> should equal (System.DateTime(1957, 8, 14))


type elmWithChildChoice = XmlProviderFromSchema<"""
  <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" 
    elementFormDefault="qualified" attributeFormDefault="unqualified">
      <xs:element name="foo">
		<xs:complexType>
			<xs:choice>
				<xs:element name="bar" type="xs:int"/>
				<xs:element name="baz" type="xs:date"/>
			</xs:choice>
		</xs:complexType>
	</xs:element>
  </xs:schema>""", ElementName = "foo">

[<Test>]
let ``element with child choice``() =
  let foo = elmWithChildChoice.Parse("""
  <foo>
    <baz>1957-08-13</baz>
  </foo>""")
  foo.Bar |> should equal None
  foo.Baz |> should equal (Some <| System.DateTime(1957, 8, 13))
  
  let foo = elmWithChildChoice.Parse("""
  <foo>
    <bar>5</bar>
  </foo>""")
  foo.Bar |> should equal (Some 5)
  foo.Baz |> should equal None




type schema = XmlProviderFromSchema<"""<schema xmlns="http://www.w3.org/2001/XMLSchema" targetNamespace="https://github.com/FSharp.Data/" xmlns:tns="https://github.com/FSharp.Data/" attributeFormDefault="unqualified" >
  <complexType name="root">
    <sequence>
      <element name="elem" type="string" >
        <annotation>
          <documentation>This is an identification of the preferred language</documentation>
        </annotation>
      </element>
      <element name="elem1" type="tns:foo" />
      <element name="choice" type="tns:bar" maxOccurs="2" />
      <element name="anonymousTyped">
        <complexType>
          <sequence>
            <element name="covert" type="boolean" />
          </sequence>
          <attribute name="attr" type="string" />
          <attribute name="windy">
            <simpleType>
              <restriction base="string">
                <maxLength value="10" />
              </restriction>
            </simpleType>
          </attribute>
        </complexType>
      </element>
    </sequence>
  </complexType>
  <complexType name="bar">
    <choice>
      <element name="language" type="string" >
        <annotation>
          <documentation>This is an identification of the preferred language</documentation>
        </annotation>
      </element>
      <element name="country" type="integer" />
      <element name="snur">
        <complexType>
          <sequence>
            <element name ="baz" type ="string"/>
          </sequence>
        </complexType>
      </element>
    </choice>
  </complexType>
  <complexType name="foo">
    <sequence>
      <element name="fooElem" type="boolean" />
      <element name="ISO639Code">
        <annotation>
          <documentation>This is an ISO 639-1 or 639-2 identifier</documentation>
        </annotation>
        <simpleType>
          <restriction base="string">
            <maxLength value="10" />
          </restriction>
        </simpleType>
      </element>
    </sequence>
  </complexType>
  <element name="rootElm" type="tns:root" />
</schema>""", ElementName = "rootElm", ElementNamespace = @"https://github.com/FSharp.Data/">

[<Test>]
let ``Simple schema``() =
    let xml = 
         """<?xml version="1.0" encoding="utf-8"?>
              <tns:rootElm xmlns:tns="https://github.com/FSharp.Data/">
                <elem>it starts with a number</elem>
                <elem1>
                  <fooElem>false</fooElem>
                  <ISO639Code>dk-DA</ISO639Code>
                </elem1>
                <choice>
                  <language>Danish</language>
                </choice>
                <choice>
                  <country>1</country>
                </choice>
                <anonymousTyped attr="fish" windy="strong" >
                  <covert>True</covert>
                </anonymousTyped>
              </tns:rootElm>
         """ 
    let root = schema.Parse xml
    let choices = root.Choices
    choices.[1].Country.Value     |> should equal "1" // an integer is mapped to string (BigInteger would be better)
    choices.[0].Language.Value    |> should equal "Danish"
    root.AnonymousTyped.Covert    |> should equal true
    root.AnonymousTyped.Attr      |> should equal (Some "fish")
    root.AnonymousTyped.Windy     |> should equal (Some "strong")
//    root.Metadata.TargetNamespace |> should equal "https://github.com/FSharp.Data/"
//    root.Metadata.TypeName        |> should equal "root"





