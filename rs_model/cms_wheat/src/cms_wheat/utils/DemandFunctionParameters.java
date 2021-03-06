package cms_wheat.utils;

import cms_wheat.Cms_builder;


public class DemandFunctionParameters{
	int intercept,slope;
	String market,producer;

	public DemandFunctionParameters(int theIntercept,String theMarket,String theProducer){
		intercept=theIntercept;
		market=theMarket;
		producer=theProducer;
//		if(Cms_builder.verboseFlag){System.out.println("          new parameters; demand at p=0: "+intercept+", market: "+market+", producer: "+producer);}
	}
	public void setSlope(double thisDemandFunctionParametersSlope){
		slope=(int)thisDemandFunctionParametersSlope;
		if(Cms_builder.verboseFlag){System.out.println("                 parameter slope changed; demand at p=0: "+intercept+" at p="+(2*Cms_builder.demandFunctionReferencePrice)+": "+(intercept-slope*2*Cms_builder.demandFunctionReferencePrice)+", market: "+market+", producer: "+producer);System.out.println("                       i.e. decreasing "+slope+" every 1 unit increase of price");}
//System.out.println("                 parameter slope changed; demand at p=0: "+intercept+" at p=10 "+(intercept-slope*10)+", market: "+market+", producer: "+producer);
	}

	public void setIntercept(int theNewIntercept){
		intercept=theNewIntercept;
//		if(Cms_builder.verboseFlag){System.out.println("                 demand at p=0 set to "+theNewIntercept+", market: "+market+", producer: "+producer);}
	}
	public void increaseInterceptBy(int interceptIncrease){
		intercept+=interceptIncrease;
		if(Cms_builder.verboseFlag){System.out.println("                 demand increased by "+interceptIncrease+", market: "+market+", producer: "+producer);}
//		System.out.println("                 demand increased by "+interceptIncrease+", market: "+market+", producer: "+producer);
	}
	public void decreaseInterceptBy(int interceptDecrease){
		intercept+=-interceptDecrease;
		if(Cms_builder.verboseFlag){System.out.println("                 demand decreased by "+interceptDecrease+", market: "+market+", producer: "+producer);}
//		System.out.println("                 demand decreased by "+interceptDecrease+", market: "+market+", producer: "+producer);
	}
	public void slopeNotChanged(){
		if(Cms_builder.verboseFlag){System.out.println("                 parameter slope not changed");}
	}

	public int getIntercept(){
		return intercept;
	}
	public String getMarketName(){
		return market;
	}
	public String getProducerName(){
		return producer;
	}
	public int getSlope(){
		return slope;
	}

}
