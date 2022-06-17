input TEX;
prologues:=3;
beginfig(1);
u:=-0.5cm;
y:=0.5cm;    %height of a row 
j:=0;        %livello inferiore della riga che si sta considerando. dunque, il livello superiore e' j+y 
du:=0.23cm;
dcol:=5mm;
ldis:=0.5mm;
scalelabels:=0.4;
scalelabels1:=0.2;
scalelabels2:=0.6;

string area[];
input mpinput_areas_effects;
for i=1 upto 11:
	label.lft(TEX(area[i]) scaled scalelabels2,(u,-i*y));
endfor

%draw nino effects on production quantities
u:=-0.0cm;
label(btex effect etex scaled scalelabels2 rotated 90,(u,0.5mm));
for i=1 upto 11:
	if ro[i]=0 :
	else:
	draw (u-du,-i*y-du)--(u+du,-i*y-du)--(u+du,-i*y+du)--(u-du,-i*y+du)--cycle;
		if ro[i]>0:
	 		fill (u-du,-i*y-du)--(u-du,-i*y+du)--(u+du,-i*y+du)--(u+du,-i*y-du)--cycle withcolor (1-ro[i],1-ro[i],1.0);
			 if ro[i]>0.5 : 
	 			label(decimal(o[i]) infont defaultfont scaled scalelabels,(u,-i*y+2ldis)) withcolor white;
				label(decimal(fo[i]) infont defaultfont scaled 1.5scalelabels1,(u,-i*y-1*ldis)) withcolor white;
				label("+" infont defaultfont scaled 1.5scalelabels1,(u+3*ldis,-i*y-1*ldis)) withcolor white;
				label(decimal(so[i]) infont defaultfont scaled 1.5scalelabels1,(u,-i*y-3*ldis)) withcolor white;
	 		else:
	 			label(decimal(o[i]) infont defaultfont scaled scalelabels,(u,-i*y+2ldis));
				label(decimal(fo[i]) infont defaultfont scaled 1.5scalelabels1,(u,-i*y-1*ldis));
				label("+" infont defaultfont scaled 1.5scalelabels1,(u+3*ldis,-i*y-1*ldis));
				label(decimal(so[i]) infont defaultfont scaled 1.5scalelabels1,(u,-i*y-3*ldis));
 			fi

		else:
			fill (u-du,-i*y-du)--(u-du,-i*y+du)--(u+du,-i*y+du)--(u+du,-i*y-du)--cycle  withcolor (1,1+ro[i],1+ro[i]);
			label(decimal(o[i]) infont defaultfont scaled scalelabels,(u,-i*y+2*ldis));
			label(decimal(fo[i]) infont defaultfont scaled 1.5scalelabels1,(u,-i*y-1*ldis));
			label("+" infont defaultfont scaled 1.5scalelabels1,(u+3*ldis,-i*y-1*ldis));
			label(decimal(so[i]) infont defaultfont scaled 1.5scalelabels1,(u,-i*y-3*ldis));
		fi
	fi
endfor

%draw nino effects on import and prices

u:=0.5cm;  %width of a month
input mpinput1nino;
input mpdrawsquares;
label(btex 2013 etex scaled scalelabels2 rotated 90,(u,0));

u:=u+dcol;
input mpinput2nino;
input mpdrawsquares;
label(btex 2014 etex scaled scalelabels2 rotated 90,(u,0));

u:=u+dcol;
input mpinput3nino;
input mpdrawsquares;
label(btex 2015 etex scaled scalelabels2 rotated 90,(u,0));

u:=u+dcol;
input mpinput4nino;
input mpdrawsquares;
label(btex 2016 etex scaled scalelabels2 rotated 90,(u,0));

%draw nina effects on production quantities
u:=3.0cm;
label(btex effect etex scaled scalelabels2 rotated 90,(u,0.5mm));
for i=1 upto 11:
	if ra[i]=0 :
	else:
	draw (u-du,-i*y-du)--(u+du,-i*y-du)--(u+du,-i*y+du)--(u-du,-i*y+du)--cycle;
		if ra[i]>0:
	 		fill (u-du,-i*y-du)--(u-du,-i*y+du)--(u+du,-i*y+du)--(u+du,-i*y-du)--cycle withcolor (1-ra[i],1-ra[i],1.0);
			 if ra[i]>0.5 : 
	 			label(decimal(a[i]) infont defaultfont scaled scalelabels,(u,-i*y+2ldis)) withcolor white;
				label(decimal(fa[i]) infont defaultfont scaled 1.5scalelabels1,(u,-i*y-1*ldis)) withcolor white;
				label("+" infont defaultfont scaled 1.5scalelabels1,(u+3*ldis,-i*y-1*ldis)) withcolor white;
				label(decimal(sa[i]) infont defaultfont scaled 1.5scalelabels1,(u,-i*y-3*ldis)) withcolor white;
	 		else:
	 			label(decimal(a[i]) infont defaultfont scaled scalelabels,(u,-i*y+2ldis));
				label(decimal(fa[i]) infont defaultfont scaled 1.5scalelabels1,(u,-i*y-1*ldis));
				label("+" infont defaultfont scaled 1.5scalelabels1,(u+3*ldis,-i*y-1*ldis));
				label(decimal(sa[i]) infont defaultfont scaled 1.5scalelabels1,(u,-i*y-3*ldis));
 			fi

		else:
			fill (u-du,-i*y-du)--(u-du,-i*y+du)--(u+du,-i*y+du)--(u+du,-i*y-du)--cycle  withcolor (1,1+ra[i],1+ra[i]);
			label(decimal(a[i]) infont defaultfont scaled scalelabels,(u,-i*y+2*ldis));
				label(decimal(fa[i]) infont defaultfont scaled 1.5scalelabels1,(u,-i*y-1*ldis));
				label("+" infont defaultfont scaled 1.5scalelabels1,(u+3*ldis,-i*y-1*ldis));
				label(decimal(sa[i]) infont defaultfont scaled 1.5scalelabels1,(u,-i*y-3*ldis));
		fi
	fi
endfor

%draw nina effects on import and prices

u:=3.5cm;  %width of a month
input mpinput1nina;
input mpdrawsquares;
label(btex 2013 etex scaled 0.5 rotated 90,(u,0));

u:=u+dcol;
input mpinput2nina;
input mpdrawsquares;
label(btex 2014 etex scaled 0.5 rotated 90,(u,0));

u:=u+dcol;
input mpinput3nina;
input mpdrawsquares;
label(btex 2015 etex scaled 0.5 rotated 90,(u,0));

u:=u+dcol;
input mpinput4nina;
input mpdrawsquares;
label(btex 2016 etex scaled 0.5 rotated 90,(u,0));



label(btex El Ni\~no etex scaled scalelabels2,(1.25cm,-12*y));
label(btex La Ni\~na etex scaled scalelabels2,(4.25cm,-12*y));

z10 = llcorner currentpicture;  y10 := y10-10pt;x10 := x10-10pt;
z30 = urcorner currentpicture;  y30 := y30-q0pt;x30 := x30+10pt;
z20 = lrcorner currentpicture;
z40 = ulcorner currentpicture;
%z20 = (x30, y10);
%z40 = (x10, x30);
%setbounds currentpicture to z10--z20--z30--z40--cycle;
setbounds currentpicture to (-2.5cm,-6.3cm)--(5.5cm,-6.3cm)--(5.5cm,0.5cm)--(-2.5cm,0.5cm)--cycle;

endfig;
end;
