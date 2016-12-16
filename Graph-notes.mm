<map version="1.0.1">
<!-- To view this file, download free mind mapping software FreeMind from http://freemind.sourceforge.net -->
<node CREATED="1479307928543" ID="ID_115580742" MODIFIED="1479308216259" TEXT="Graph">
<font NAME="SansSerif" SIZE="16"/>
<node CREATED="1479307978656" ID="ID_1467790846" MODIFIED="1479308216235" POSITION="right" TEXT="">
<font NAME="SansSerif" SIZE="16"/>
</node>
<node CREATED="1479308005979" ID="ID_1461725970" MODIFIED="1479308216211" POSITION="left" TEXT="invariants">
<font NAME="SansSerif" SIZE="16"/>
<node CREATED="1479308061764" ID="ID_378722052" MODIFIED="1479308216188" TEXT="scope">
<font NAME="SansSerif" SIZE="16"/>
<node CREATED="1479308065663" ID="ID_939090742" MODIFIED="1479308216164" TEXT="Input">
<font NAME="SansSerif" SIZE="16"/>
</node>
<node CREATED="1479308091707" ID="ID_1563514202" MODIFIED="1479308216094" TEXT="Output">
<font NAME="SansSerif" SIZE="16"/>
</node>
<node CREATED="1479308095316" ID="ID_1340973971" MODIFIED="1479308233811" TEXT="Transformation">
<font NAME="SansSerif" SIZE="16"/>
<node CREATED="1479308237795" ID="ID_1309060989" MODIFIED="1479308254807" TEXT="like -&gt;i">
<font NAME="DejaVu Sans Mono" SIZE="12"/>
</node>
</node>
</node>
<node CREATED="1479308274575" ID="ID_1586701005" MODIFIED="1479308282949" TEXT="Verification time">
<node CREATED="1479308293860" ID="ID_46833274" MODIFIED="1479308297548" TEXT="Run-time"/>
<node CREATED="1479308298132" ID="ID_170224426" MODIFIED="1479308304972" TEXT="Compile-time">
<node CREATED="1479308308988" ID="ID_1071628135" MODIFIED="1479308321217" TEXT="Field types"/>
<node CREATED="1479308325655" ID="ID_1497263973" MODIFIED="1479308474005" TEXT="Type policy">
<node CREATED="1479308366808" ID="ID_1203881216" MODIFIED="1479308368145" TEXT="e.g. no cycles within the types"/>
</node>
<node CREATED="1479308372692" ID="ID_1614080405" MODIFIED="1479308442436" TEXT="Macro policy">
<node CREATED="1479308444994" ID="ID_1687400809" MODIFIED="1479308450313" TEXT="i.e. correct by construction"/>
<node CREATED="1479308486419" FOLDED="true" ID="ID_1005390685" MODIFIED="1479308873487" TEXT="May interfere with each other">
<icon BUILTIN="messagebox_warning"/>
<node CREATED="1479308564613" ID="ID_246345249" MODIFIED="1479308870465" STYLE="bubble" TEXT="e.g. a &quot;no cycles starting from this node&quot; constraint&#xa;would not work as expected if a &quot;backwards link&quot; is&#xa;filled in afterwards.&#xa;We probably need to hardcode a basic set of&#xa;constraints which know about each other and&#xa;about the potential interactions."/>
</node>
<node CREATED="1479308516231" FOLDED="true" ID="ID_521874707" MODIFIED="1479309080946" TEXT="May alter a mapping&apos;s inputs">
<node CREATED="1479308984967" ID="ID_666428602" MODIFIED="1479309027685" TEXT="Conserve well-scopedness within a transition:&#xa;pass in nodes flagged with a &#x2200; type, and&#xa;check that the output contains that flag.&#xa;Potentially out-of-scope fields in the input do&#xa;not have the flag."/>
</node>
<node CREATED="1479308767829" ID="ID_1128145279" MODIFIED="1479308903893" TEXT="May wrapp a mapping&apos;s outputs">
<node CREATED="1479308905956" ID="ID_1856836622" MODIFIED="1479308919205" TEXT="e.g. wrap with (ann)"/>
</node>
</node>
</node>
</node>
<node CREATED="1479308289261" ID="ID_1896946451" MODIFIED="1479308292028" TEXT="Specification"/>
</node>
</node>
</map>
