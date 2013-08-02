<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:wc="http://worldcat.org/xid/isbn/" version="1.0">
    <xsl:output method="text" omit-xml-declaration="yes" indent="no"/>
    <xsl:template match="wc:isbn">
        <code>
    @BOOK{CiteKeyGoesHere,
        AUTHOR = "<xsl:value-of select="@author"/>",
        TITLE = "<xsl:value-of select="@title"/>",
        PUBLISHER = "<xsl:value-of select="@publisher"/>",
        ADDRESS = "<xsl:value-of select="@city"/>",
        YEAR ="<xsl:value-of select="@year"/>"}
</code>
    </xsl:template>
</xsl:stylesheet>
