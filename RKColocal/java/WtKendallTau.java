
public class WtKendallTau {
	
	public double[] X;
	public double[] Y;
	public double[] W;
	
	public WtKendallTau(double[]InputX, double[]InputY, double[]InputW) {
		X = InputX;
		Y = InputY;
		W = InputW;
	}
	
	public double calculate() {
		
		double[][] rankedData = rank(X, Y, W);
		int[] rankedindex = new int[X.length];
		double[] rankedw = new double[X.length];
		
		for(int i = 0; i < X.length; i++)
		{
			rankedindex[i] = (int)rankedData[i][0];
			rankedw[i] = rankedData[i][2];
		}
		
		final MergeSort mergeSort = new MergeSort(rankedindex, rankedw, new IntComparator() {

			@Override
			public int compare(int a, int b) {
				return Integer.compare(a, b);
			}
		});
		
		double swap = mergeSort.sort();
		double tw = totw(W)/2;
		
		double tau = (tw - 2 * swap) / tw;
		
		return tau;	
	}
	
    public double brutalcalculate() {
		
		double[][] rankedData = rank(X, Y, W);
		int[] rankedindex = new int[X.length];
		double[] rankedw = new double[X.length];
		double sumw = 0;
		double sumnum = 0;
		double tempw;
		double temp;
		
		for(int i = 0; i < X.length; i++)
		{
			for (int j = i+1; j < X.length; j++)
			{
				tempw = rankedData[i][2] * rankedData[j][2];
				temp = (rankedData[i][0] - rankedData[j][0]) * (rankedData[i][1] - rankedData[j][1]);
				if (temp > 0)
				{
					sumnum = sumnum + tempw;
				}
				else
				{
					sumnum = sumnum - tempw;
				}
				sumw = sumw + tempw;
			}
		}
		
		
		double tau = sumnum / sumw;
		
		return tau;	
	}
	
	private double totw(double[] w) {
		double sumw = 0;
		double sumsquarew = 0;
		
		for (int i = 0; i < w.length; i++)
		{
			sumw += w[i];
			sumsquarew += w[i]*w[i];
		}
		
		double result = sumw * sumw - sumsquarew;
		
		return result;
	}
	
	private double[][] rank(double[] IX, double[] IY, double[] IW) {
		double[][] combinedData = new double[IX.length][3];
		
		for(int i = 0; i < IX.length; i++)
		{
			combinedData[i][0] = IX[i];
			combinedData[i][1] = IY[i];
			combinedData[i][2] = IW[i];
		}
		
		//sort X
		java.util.Arrays.sort(combinedData, new java.util.Comparator<double[]>() {
			@Override
			public int compare(double[] row1, double[] row2) {
				return Double.compare(row1[0], row2[0]);
			}
		});
		
		int start = 0;
		int end = 0;
		int rank=0;
		while (end < IX.length-1)
		{
			while (Double.compare(combinedData[start][0],combinedData[end][0]) == 0)
			{
				end++;
				if(end >= IX.length)
					break;
			}
			for (int i = start; i < end; i++){
				combinedData[i][0]=rank+Math.random();
			}
			rank++;
			start=end;
		}
		
		java.util.Arrays.sort(combinedData, new java.util.Comparator<double[]>() {
			@Override
			public int compare(double[] row1, double[] row2) {
				return Double.compare(row1[0], row2[0]);
			}
		});
		
		for (int i = 0; i < IX.length; i++) {
			combinedData[i][0] = i + 1;
		}
		
		//sort Y
		java.util.Arrays.sort(combinedData, new java.util.Comparator<double[]>() {
			@Override
			public int compare(double[] row1, double[] row2) {
				return Double.compare(row1[1], row2[1]);
			}
		});
		
		start = 0;
		end = 0;
		rank=0;
		while (end < IX.length-1)
		{
			while (Double.compare(combinedData[start][1],combinedData[end][1]) == 0)
			{
				end++;
				if(end >= IX.length)
					break;
			}
				
			for (int i = start; i < end; i++){
				combinedData[i][1]=rank+Math.random();
			}
			rank++;
			start=end;
		}
		
		java.util.Arrays.sort(combinedData, new java.util.Comparator<double[]>() {
			@Override
			public int compare(double[] row1, double[] row2) {
				return Double.compare(row1[1], row2[1]);
			}
		});
		
		for (int i = 0; i < IX.length; i++) {
			combinedData[i][1] = i + 1;
		}
		
		return combinedData;
	}
	
	private final static class MergeSort {

		private int[] index;
		private double[] w;
		private final IntComparator comparator;

		public MergeSort(int[] index, double[] w, IntComparator comparator) {
			this.index = index;
			this.w = w;
			this.comparator = comparator;
		}

//		public int[] getSorted() {
//			return index;
//		}
		
		public double sort() {
			  double swap = 0;
			  double tempswap;
			  int n = index.length;
			  int step = 1;
			  int[] index1 = new int[n];
			  int[] index2 = new int[n];
			  double[] w1 = new double[n];
			  double[] w2 = new double[n];
			  double[] cumw = new double[n];
			  int begin;
			  int begin2;
			  int end;
			  int k;
			  
			  for (int i = 0; i < n; i++)
			  {
			    index1[i] = index[i];
			    w1[i] = w[i];
			  }
			  
			  while (step < n) {
			    begin=0;
			    k=0;
			    cumw[0]=w1[0];
			    for (int i=1;i<n;i++)
			    {
			      cumw[i]=cumw[i-1]+w1[i];
			    }
			    
			    while (true)
			    {
			      begin2 = begin + step;
			      end = begin2 + step;
			      if (end > n)
			      {
			        if (begin2 > n)
			          break;
			        end = n;
			      }
			      int i = begin;
			      int j = begin2;
			      while (i < begin2 && j < end)
			      {
			        if (comparator.compare(index1[i], index1[j])>0)
			        {
			          if (i == 0)
			          {
			            tempswap = w1[j]*cumw[begin2-1];
			          } else {
			            tempswap = w1[j]*(cumw[begin2-1]-cumw[i-1]);
			          }
			          swap = swap + tempswap;
			          index2[k] = index1[j];
			          w2[k++] = w1[j++];
			        }
			        else
			        {
			          index2[k] = index1[i];
			          w2[k++] = w1[i++];
			        }
			      }
			      if (i < begin2)
			      {
			        while (i < begin2)
			        {
			          index2[k] = index1[i];
			          w2[k++] = w1[i++];
			        }
			      } else {
			        while (j < end)
			        {
			          index2[k] = index1[j];
			          w2[k++] = w1[j++];
			        }
			      }
			      begin = end;
			    }
			    if (k < n)
			    {
			      while(k < n)
			      {
			        index2[k] = index1[k];
			        w2[k] = w1[k];
			        k++;
			      }
			    }
			    for (int i = 0; i < n; i++)
			    {
			      index1[i] = index2[i];
			      w1[i] = w2[i];
			    } 
			    
			    step *= 2;
			  }
			  return swap;
		}
	}

}
