BEGIN   { line=0; FS="";
    out=ARGV[ARGC-1]  ".out";
    var=ARGV[ARGC-1]  ".var";
    ext=ARGV[ARGC-1]  ".ext";
    def=ARGV[ARGC-1]  ".def";
    inc=ARGV[ARGC-1]  ".inc";
    typ=ARGV[ARGC-1]  ".typ";
    system ( rm " " -f " " out " " var " " ext " " def " " inc " " typ );
    }
/^[     ]*\/\/.*/   { print "comment :" $0 "\n"; print $0 >> out ; next ;}
/^#define.*/        { print "define :" $0 ; print $0 >>def ; next;}
/^#include.*/       { print "define :" $0 ; print $0 >>inc ; next;}
/^typedef.*{$/      { print "typedef var :" $0 "\n"; decl="typedef";print $0 >> typ;infile="typ";next;}
/^extern.*$/        { print "extern :" $0 "\n"; print $0 >> ext;infile="ext";next;}
/^[^    }].*{$/     { print "init var :" $0 "\n";decl="var";print $0 >> var; infile="vars";
                print $0;
                fout=gensub("^([^    \\*])*[    ]*([a-zA-A0-9_]*)\\[.*","\\2","g") ".vars";
                     print "var decl : " $0 "in file " fout;
                     print $0 >fout;
                next;
                        }
/^[^    }].*)$/     { print "func  :" $0 "\n";decl="func"; infile="func";
                print $0;
                fout=gensub("^.*[    \\*]([a-zA-A0-9_]*)[   ]*\\(.*","\\1","g") ".func";
                     print "function : " $0 "in file " fout;
                     print $0 >fout;
                next;
            }
/^}[    ]*$/        { print "end of " decl ":" $0 "\n";
                if(infile=="typ") {
                    print $0 >> typ;
                }else if (infile=="ext"){
                    print $0 >> ext;
                }else if (infile=="var") {
                    print $0 >> var;
                }else if ((infile=="func")||(infile=="vars")) {
                    print $0 >> fout;
                    fflush (fout);
                    close (fout);
                }else if (infile=="def") {
                    print $0 >> def;
                }else if (infile=="inc"){
                    print $0 >> inc;
                }else print $0 >> out;
                next;
            }
/^[a-zA-Z_]/        { print "extern :" $0 "\n"; print $0 >> var;infile="var";next;}
            { print "other :" $0 "\n" ;
                if(infile=="typ") {
                    print $0 >> typ;
                }else if (infile=="ext"){
                    print $0 >> ext;
                }else if (infile=="var") {
                    print $0 >> var;
                }else if ((infile=="func")||(infile=="vars")){
                    print $0 >> fout;
                }else if (infile=="def") {
                    print $0 >> def;
                }else if (infile=="inc"){
                    print $0 >> inc;
                }else print $0 >> out;
               next;
               }
