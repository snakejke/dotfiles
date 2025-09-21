{
    programs.texlive = {
    enable = true;
    extraPackages = tpkgs: {
      inherit (tpkgs)
        collection-basic
        collection-latex
        #collection-fontsextra
        
        babel-russian
        hyphen-russian
        cyrillic
        lh
        #cm-super
        luahyphenrules

        svg
        moderncv
        fontawesome
        
        geometry
        enumitem
        hyperref
        xcolor
        graphics
        fancyvrb
        
        dvisvgm
        dvipng
        
        tools
        oberdiek
        scheme-medium 
        titlesec

        wrapfig
        multirow
        ulem
        booktabs
        amsmath
        capt-of
        amsfonts;
      
    };
  };
}
