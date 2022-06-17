for i=1 upto 11:
	draw (u-du,-i*y-du)--(u+du,-i*y-du)--(u+du,-i*y+du)--(u-du,-i*y+du)--cycle;
if rpf[i]>0 :
	fill (u-du,-i*y-du)--(u+du,-i*y-du)--(u+du,-i*y+du)--cycle  withcolor (1,1-rpf[i],1-rpf[i]);
	label(decimal(pf[i]) infont defaultfont scaled scalelabels rotated 45,(u+ldis,-i*y-ldis));
else:
	fill (u-du,-i*y-du)--(u+du,-i*y-du)--(u+du,-i*y+du)--cycle  withcolor (1+rpf[i],1+rpf[i],1);
	 if rpf[i]<-0.5 : 
	label(decimal(pf[i]) infont defaultfont scaled scalelabels rotated 45,(u+ldis,-i*y-ldis)) withcolor white;
	 else:
	label(decimal(pf[i]) infont defaultfont scaled scalelabels rotated 45,(u+ldis,-i*y-ldis));
	fi
fi
if rqf[i]>0 :
	 fill (u-du,-i*y-du)--(u-du,-i*y+du)--(u+du,-i*y+du)--cycle withcolor (1-rqf[i],1-rqf[i],1.0);
	 if rqf[i]>0.5 : 
	 label(decimal(qf[i]) infont defaultfont scaled scalelabels rotated 45,(u-ldis,-i*y+ldis)) withcolor white;
	 else:
	 label(decimal(qf[i]) infont defaultfont scaled scalelabels rotated 45,(u-ldis,-i*y+ldis));
 	fi
else:
	fill (u-du,-i*y-du)--(u-du,-i*y+du)--(u+du,-i*y+du)--cycle  withcolor (1,1+rqf[i],1+rqf[i]);
	label(decimal(qf[i]) infont defaultfont scaled scalelabels rotated 45,(u-ldis,-i*y+ldis));
fi

if rcf[i]=rqf[i] :
else:
	if rcf[i]>0 :
	fill (u-du,-i*y)--(u-du,-i*y+du)--(u,-i*y+du)--cycle withcolor (1-rcf[i],1-rcf[i],1.0);
		if rcf[i]>0.5 : 
		label(decimal(cf[i]) infont defaultfont scaled scalelabels1 rotated 45,(u-3*ldis,-i*y+3*ldis)) withcolor white;
		else:
		label(decimal(cf[i]) infont defaultfont scaled scalelabels1 rotated 45,(u-3*ldis,-i*y+3*ldis));
		fi
	else:
	fill (u-du,-i*y)--(u-du,-i*y+du)--(u,-i*y+du)--cycle withcolor (1,1+rcf[i],1+rcf[i]);
	label(decimal(cf[i]) infont defaultfont scaled scalelabels1 rotated 45,(u-3*ldis,-i*y+3*ldis));
	fi
fi
endfor


