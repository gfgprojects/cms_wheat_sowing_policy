package cms_wheat.utils;
import cms_wheat.agents.Market;

import java.util.Comparator;

public class MarketLongitudeComparator implements Comparator<Market> {

public int compare(Market market1,Market market2){
	return (int)Math.round(market2.getLongitude()-market1.getLongitude());
}
}
