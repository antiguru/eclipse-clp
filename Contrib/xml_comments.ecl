:- comment(summary,'A bi-directional XML parser').
:- comment(author, "John Fletcher, ECLiPSe wrapper by Joachim Schimpf").
:- comment(copyright, "Copyright (C) 2001, 2002 Binding Time Limited").
:- comment(desc, html("
<h2>Note for ECLiPSe users</h2>
<p>
This code creates and accepts character lists rather than ECLiPSe strings. 
To convert between character lists and (UTF8 or ASCII) strings, use the
ECLiPSe built-in string_list/3. For example, to parse a UTF-8 encoded
XML file, use the following code:
</p><pre>
xml_parse_file(File, Document) :-
	open(File, read, Stream),
	read_string(Stream, end_of_file, _, Utf8String),
	close(Stream),
	string_list(Utf8String, Chars, utf8),
	xml_parse(Chars, Document).
</pre><p>
Most of the subsequent text is taken literally from
<a href=\"http://www.john.fletcher.dial.pipex.com/xml.pl.shtml\">
http://www.john.fletcher.dial.pipex.com/xml.pl.shtml</a>.
</p>


<h2>TERMS AND CONDITIONS</h2>
<p>
This program is offered free of charge, as unsupported source code. You may
use it, copy it, distribute it, modify it or sell it without restriction. 
</p><p>
We hope that it will be useful to you, but it is provided \"as is\" without
any warranty express or implied, including but not limited to the warranty
of non-infringement and the implied warranties of merchantability and fitness
for a particular purpose.
</p><p>
Binding Time Limited will not be liable for any damages suffered by you as
a result of using the Program. In no event will Binding Time Limited be
liable for any special, indirect or consequential damages or lost profits
even if Binding Time Limited has been advised of the possibility of their
occurrence. Binding Time Limited will not be liable for any third party
claims against you.
</p>
<pre>
History:
$Log: xml_comments.ecl,v $
Revision 1.1  2006/09/23 01:45:21  snovello
Initial revision

Revision 1.1  2003/03/31 13:58:02  js10
Upgraded to latest version from John Fletcher's web site

Revision 1.2  2002/03/26 22:56:55  js10
Added John Fletcher's public domain XML parser/generator

Revision 1.1  2002/03/26 22:50:07  js10
Added John Fletcher's public domain XML parser/generator

Revision 1.1  2002-01-31 21:04:45+00  john
Updated Copyright statements.

Revision 1.0  2001-10-17 20:46:24+01  john
Initial revision
</pre>

  <h2>Background</h2>
  <p>xml.pl is a module for parsing <acronym title=\"eXtensible Markup Language\">
XML</acronym> with Prolog, which provides Prolog applications with a simple
interface to XML documents. We have used it successfully in a number of
applications.</p>
  <p>It supports a subset of XML suitable for XML Data and Worldwide Web
  applications. It is neither as strict nor as comprehensive as the <a href=\"http://www.w3.org/TR/2000/REC-xml-20001006\">XML 1.0 Specification</a>
mandates.</p>
  <p>It is not as strict, because, while the specification must eliminate
ambiguities, not all errors need to be regarded as faults, and some reasonable
examples of real XML usage would have to be rejected if they were.</p>
  <p>It is not as comprehensive, because, where the XML specification makes
  provision for more or less complete <acronym title=\"Document Type Declaration\">DTD</acronym>s to be provided as part of a
document, xml.pl supports the local definition of ENTITIES only.</p>
  <p>We have placed <a href=\"xml_download.shtml\">the code, and a small Windows
application which embodies it</a>, into the public domain, to encourage the use
of Prolog with XML.</p>
  <p>We hope that they will be useful to you, but they are not supported, and
Binding Time Limited accept NO LIABILITY WHATSOEVER in respect of their
use.</p>
  <h2>Specification</h2>
  <p>Three predicates are exported by the module: xml_parse/[2,3], xml_subterm/2
and xml_pp/1.</p>
  <p><dfn>xml_parse( {+Controls}, +?Chars, ?+Document )</dfn> parses <var>
Chars</var>, a list of character codes, to/from a data structure of the form
<code class=\"Contrast\">
xml(&lt;attributes&gt;, &lt;content&gt;)
</code> , where:</p>
  <p><code class=\"Contrast\">&lt;attributes&gt;</code> is a list of <code class=\"Contrast\">
 &lt;name&gt;=&lt;data&gt;
</code> attributes from the (possibly implicit) XML signature of the
document.</p>
  <p><code class=\"Contrast\">&lt;content&gt;</code> is a (possibly empty) list comprising occurrences of :</p>
  <table>
   <tr>
    <td><code class=\"Contrast\">pcdata(&lt;data&gt;)</code></td>
    <td>Text</td>
   </tr>
   <tr>
    <td><code class=\"Contrast\"> comment(&lt;string&gt;)</code></td>
    <td>An xml comment;</td>
   </tr>
   <tr>
    <td><code class=\"Contrast\">namespace(&lt;URI&gt;,&lt;prefix&gt;,&lt;element&gt;)</code></td>
    <td>a Namespace</td>
   </tr>
   <tr>
    <td><code class=\"Contrast\"> element(&lt;tag&gt;, &lt;attributes&gt;, &lt;content&gt;)</code></td>
    <td>&lt;tag&gt;..&lt;/tag&gt; encloses &lt;content&gt; or &lt;tag /&gt; if empty</td>
   </tr>
   <tr>
    <td><code class=\"Contrast\"> instructions(&lt;name&gt;, &lt;data&gt;)</code></td>
    <td> A PI  &lt;? &lt;name&gt; &lt;data&gt; ?&gt;</td>
   </tr>
   <tr>
    <td><code class=\"Contrast\"> cdata(&lt;data&gt;)</code></td>
    <td>&lt;![CDATA[ &lt;string&gt; ]]&gt;</td>
   </tr>
   <tr>
    <td><code class=\"Contrast\"> doctype(&lt;tag&gt;, &lt;doctype id&gt;)</code></td>
    <td> DTD &lt;!DOCTYPE .. &gt;</td>
   </tr>
   <tr>
    <td colspan=\"2\">&#160;</td>
   </tr>
   <tr>
    <td colspan=\"2\"><p>The conversions are not completely symmetrical, in that weaker XML is
accepted than can be generated. Specifically, in-bound <em>(Chars -&gt;
Document)</em> parsing does not require strictly well-formed XML. If <var>
Chars</var> does not represent well-formed XML, <var>Document</var> is
instantiated to the term <code class=\"Contrast\">
malformed(&lt;attributes&gt;, &lt;content&gt;)
</code> .</p></td>
   </tr>
   <tr>
    <td colspan=\"2\">&#160;</td>
   </tr>
   <tr>
    <td colspan=\"2\"><p>The <code class=\"Contrast\">&lt;content&gt;</code> of a <code>malformed/2</code> structure can include:</p></td>
   </tr>
   <tr>
    <td colspan=\"2\">&#160;</td>
   </tr>
   <tr>
    <td><code class=\"Contrast\">unparsed( &lt;string&gt; )</code></td>
    <td>Text which has not been parsed</td>
   </tr>
   <tr>
    <td><code class=\"Contrast\">out_of_context( &lt;tag&gt; )</code></td>
    <td>&lt;tag&gt; is not closed</td>
   </tr>
   <tr>
    <td colspan=\"2\"><p>in addition to the parsed term types.</p><p>Out-bound <em>(Document -&gt; Chars)</em> parsing <em>does</em> require that
<var>Document</var> defines well-formed XML. If an error is detected a &#39;domain&#39;
exception is raised.</p><p>The domain exception will attempt to identify the particular sub-term in
error and the message will show a list of its ancestor elements in the form
<code>&lt;tag&gt;{(id)}*</code> where <code>
&lt;id&gt;
</code> is the value of any attribute <em>named</em> id.</p><p>At this release, the Controls applying to in-bound <em>(Chars -&gt;
Document)</em> parsing are:</p></td>
   </tr>
   <tr>
    <td colspan=\"2\">&#160;</td>
   </tr>
   <tr>
    <td><code class=\"Contrast\">extended_characters(&lt;bool&gt;)</code></td>
    <td>Use the extended character entities for XHTML (default true)</td>
   </tr>
   <tr>
    <td><code class=\"Contrast\"> format(&lt;bool&gt;)</code></td>
    <td>Strip layouts when no non-layout character data appears between elements. default
true)</td>
   </tr>
   <tr>
    <td colspan=\"2\"><p>For out-bound <em>(Document -&gt; Chars)</em> parsing, the only available
option is:</p></td>
   </tr>
   <tr>
    <td colspan=\"2\">&#160;</td>
   </tr>
   <tr>
    <td><code class=\"Contrast\">format(&lt;bool&gt;)</code></td>
    <td>Indent the element content, (default true)</td>
   </tr>
   <tr>
    <td colspan=\"2\">&#160;</td>
   </tr>
   <tr>
    <td colspan=\"2\"><h4>Types</h4></td>
   </tr>
   <tr>
    <td><code class=\"Contrast\">&lt;tag&gt;</code></td>
    <td>An atom naming an element</td>
   </tr>
   <tr>
    <td><code class=\"Contrast\">&lt;data&gt;</code></td>
    <td>A &quot;string&quot;</td>
   </tr>
   <tr>
    <td><code class=\"Contrast\">&lt;name&gt;</code></td>
    <td>An atom, not naming an element</td>
   </tr>
   <tr>
    <td><code class=\"Contrast\">&lt;URI&gt;</code></td>
    <td>An atom giving the URI of a Namespace</td>
   </tr>
   <tr>
    <td><code class=\"Contrast\">&lt;string&gt;</code></td>
    <td>A &quot;string&quot;: list of character codes.</td>
   </tr>
   <tr>
    <td><code class=\"Contrast\">&lt;doctype id&gt;</code></td>
    <td>one of <code class=\"SourceCode\">public(&lt;string&gt;, &lt;string&gt;)</code>, <code class=\"SourceCode\">system(&lt;string&gt;)</code> or <code class=\"SourceCode\">local</code></td>
   </tr>
   <tr>
    <td><code class=\"Contrast\">&lt;bool&gt;</code></td>
    <td>one of &#39;true&#39; or &#39;false&#39;</td>
   </tr>
  </table>
  <p><dfn>xml_subterm( +XMLTerm, ?Subterm )</dfn> unifies <var>Subterm</var> with
a sub-term of <var>Term</var>. This can be especially useful when trying to
test or retrieve a deeply-nested subterm from a document. Note that <var>
XMLTerm</var> is a sub-term of itself.</p>
  <p><dfn>xml_pp( +XMLDocument )</dfn>&quot;pretty prints&quot; <var>XMLDocument</var> on
the current output stream.</p>
  <h2>Features of xml.pl</h2>
  <p>The xml/2 data structure has some useful properties.</p>
  <h3>Reusability</h3>
  <p>Using an &quot;abstract&quot; Prolog representation of XML, in which terms represent
document &quot;nodes&quot;, makes the parser reuseable for any XML application.</p>
  <p>In effect, xml.pl encapsulates the application-independent tasks of document
parsing and generation, which is essential where documents have components
from more than one Namespace.</p>
  <h3>Same Structure</h3>
  <p>The Prolog term representing a document has the same structure as the
document itself, which makes the correspondence between the literal
representation of the Prolog term and the XML source readily apparent.</p>
  <p>For example, this simple <a href=\"http://www.w3.org/Graphics/SVG/Overview.htm8\"><acronym title=\"Scalable Vector Graphics\">SVG</acronym></a> image:</p>
  <pre class=\"Contrast\">&lt;?xml version=&quot;1.0&quot; standalone=&quot;no&quot;?&gt;
&lt;!DOCTYPE svg PUBLIC &quot;-//W3C//DTD SVG 1.0//EN&quot; &quot;http://www.w3.org/.../svg10.dtd&quot;
    [
    &lt;!ENTITY redblue &quot;fill: red; stroke: blue; stroke-width: 1&quot;&gt;
    ]&gt;
&lt;svg xmlns=&quot;http://www.w3.org/2000/svg&quot; width=&quot;500&quot; height=&quot;500&quot;&gt;
 &lt;circle cx=&quot; 25 &quot; cy=&quot; 25 &quot; r=&quot; 24 &quot; style=&quot;&amp;redblue;&quot;/&gt;
&lt;/svg&gt;</pre>
  <p>... translates into this Prolog term:</p>
  <pre class=\"SourceCode\">xml( [version=&quot;1.0&quot;, standalone=&quot;no&quot;],
    [
    doctype( svg, public( &quot;-//W3C//DTD SVG 1.0//EN&quot;, &quot;http://www.w3.org/.../svg10.dtd&quot; ) ),
    namespace( &#39;http://www.w3.org/2000/svg&#39;, &quot;&quot;,
        element( svg,
            [width=&quot;500&quot;, height=&quot;500&quot;],
            [
            element( circle,
                [cx=&quot;25&quot;, cy=&quot;25&quot;, r=&quot;24&quot;, style=&quot;fill: red; stroke: blue; stroke-width: 1&quot;],
                [] )
            ] )
        )
    ] ).</pre>
  <h3>Efficient Manipulation</h3>
  <p>Each type of node in an XML document is represented by a different Prolog
functor, while data, (PCDATA, CDATA and Attribute Values), are left as
&quot;strings&quot;, (lists of character codes).</p>
  <p>The use of distinct functors for mark-up structures enables the efficient
recursive traversal of a document, while leaving the data as strings
facilitates application specific parsing of data content (aka <a href=\"http://www.google.com/search?q=%22Micro-parsing%22+XML\">
Micro-parsing</a>).</p>
  <p>For example, to turn every CDATA node into a PCDATA node with tabs expanded
into spaces:</p>
  <pre class=\"SourceCode\">
cdata_to_pcdata( cdata(CharsWithTabs), pcdata(CharsWithSpaces) ) :-
    tab_expansion( CharsWithTabs, CharsWithSpaces ).
cdata_to_pcdata( xml(Attributes, Content1), xml(Attributes, Content2) ) :-
    cdata_to_pcdata( Content1, Content2 ).
cdata_to_pcdata( namespace(URI,Prefix,Content1), namespace(URI,Prefix,Content2) ) :-
    cdata_to_pcdata( Content1, Content2 ).
cdata_to_pcdata( element(Name,Attrs,Content1), element(Name,Attrs,Content2) ) :-
    cdata_to_pcdata( Content1, Content2 ).
cdata_to_pcdata( [], [] ).
cdata_to_pcdata( [H1|T1], [H2|T2] ) :-
    cdata_to_pcdata( H1, H2 ),
    cdata_to_pcdata( T1, T2 ).
cdata_to_pcdata( pcdata(Chars), pcdata(Chars) ).
cdata_to_pcdata( comment(Chars), comment(Chars) ).
cdata_to_pcdata( instructions(Name, Chars), instructions(Name, Chars) ).
cdata_to_pcdata( doctype(Tag, DoctypeId), doctype(Tag, DoctypeId) ).
</pre>
  <p>The above uses no &#39;cuts&#39;, but will not create any choice points with ground
input.</p>
  <h3>Elegance</h3>
  <p>The resolution of entity references and the decomposition of the document
into distinct nodes means that the calling application is not concerned with
the occasionally messy syntax of XML documents.</p>
  <p>For example, the clean separation of namespace nodes means that Namespaces,
which are useful in combining specifications developed separately, have similar
usefulness in combining applications developed separately.</p>
  <p><a href=\"xml_download.shtml\">The source code is available here</a>. Although
it is unsupported, please feel free to <a href=\"mailto:xml@binding-time.co.uk\">
e-mail queries and suggestions</a>. We will respond as time allows.</p>
")).


:- comment(xml_parse/3, [
	summary:"Parse or generate XML documents",
	amode:xml_parse(+,+,-),
	amode:xml_parse(+,-,+),
	args:[
	    "Controls":"List of options",
	    "Chars":"List of characters (XML text)",
	    "Document":"Document as structured term"
	],
	see_also:[xml_parse/2],
	desc:ascii("
xml_parse( {+Controls}, +?Chars, ?+Document ) parses Chars to/from a data
structure of the form xml(<atts>, <content>). <atts> is a list of
<atom>=<string> attributes from the (possibly implicit) XML signature of the
document. <content> is a (possibly empty) list comprising occurrences of :

pcdata(<string>)		:	Text
comment(<string>)		:	An xml comment;
element(<tag>,<atts>,<content>)	:	<tag>..</tag> encloses <content>
				:       <tag /> if empty
instructions(<atom>, <string>)	:	Processing <? <atom> <params> ?>
cdata( <string> )		:	<![CDATA[ <string> ]]>
doctype(<atom>, <doctype id>)	:	DTD <!DOCTYPE .. >

The conversions are not completely symmetrical, in that weaker XML is
accepted than can be generated. Specifically, in-bound (Chars -> Document)
does not  require strictly well-formed XML. Document is instantiated to the
term malformed(Attributes, Content) if Chars does not represent well-formed
XML. The Content of a malformed/2 structure can contain:

unparsed( <string> )		:	Text which has not been parsed
out_of_context( <tag> )		:	<tag> is not closed

in addition to the standard term types.

Out-bound (Document -> Chars) parsing _does_ require that Document defines
strictly well-formed XML. If an error is detected a 'domain' exception is
raised.

The domain exception will attempt to identify the particular sub-term in
error and the message will show a list of its ancestor elements in the form
<tag>{(id)}* where <id> is the value of any attribute _named_ id.

At this release, the Controls applying to in-bound (Chars -> Document)
parsing are:

extended_characters(<bool>)	:	Use the extended character
				:	entities for XHTML (default true)

format(<bool>)			:	Strip layouts when no character data
				:	appears between elements.
				:	(default true)

[<bool> is one of 'true' or 'false']

For out-bound (Document -> Chars) parsing, the only available option is:

format(<Bool>)			:	Indent the element content
				:	(default true)

Different DCGs for input and output are used because input parsing is
more flexible than output parsing. Errors in input are recorded as part
of the data structure. Output parsing throws an exception if the document
is not well-formed, diagnosis tries to identify the specific culprit term.
")]).

:- comment(xml_parse/2, [
	summary:"Parse or generate XML documents",
	amode:xml_parse(+,-),
	amode:xml_parse(-,+),
	args:[
	    "Chars":"List of characters",
	    "Document":"Document as structured term"
	],
	see_also:[xml_parse/3,xml_subterm/2,xml_pp/1]
]).

:- comment(xml_subterm/2, [
	summary:"Unifies Subterm with a sub-term of Term.",
	amode:xml_subterm(+,?),
	args:[
	    "XMLTerm":"Structured term",
	    "Subterm":"Structured term"
	],
	see_also:[xml_parse/2,xml_parse/3,xml_pp/1]
]).


:- comment(xml_pp/1, [
	summary:"Pretty-prints XMLDocument on the current output stream",
	amode:xml_pp(+),
	args:[
	    "XMLDocument":"Document as structured term"
	],
	see_also:[xml_parse/2,xml_parse/3,xml_subterm/2]
]).

