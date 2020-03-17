
public class SmoothedKendallTau {
	
	public double[][] X;
	public double[][] Y;
	public int Bsize;
	public double ThreX;
	public double ThreY;
	
	public SmoothedKendallTau(double[][] IX, double[][] IY, int IBsize,double IThreX, double IThreY) {
		this.X = IX;
		this.Y = IY;
		this.Bsize = IBsize;
		this.ThreX = IThreX;
		this.ThreY = IThreY;
	}
	
	public double[][] execute() {
		int nr = X.length;
		int nc = X[0].length;
		double[][] result = new double[nr][nc];
		
		double[][] kernel = kernelGenerate(Bsize);
		
		int[] rowrange = new int[4];
		int[] colrange = new int[4];
		int totnum=(2*Bsize+1)*(2*Bsize+1);
		double[] LocX = new double[totnum];
		double[] LocY = new double[totnum];
		double[] LocW = new double[totnum];
		double VarW;
		double tau;
		
		for (int i = 0; i < nr; i++)
		{
			rowrange = getRange(i, Bsize, nr);
		    for (int j = 0; j < nc; j++)
		    {
		    	colrange = getRange(j, Bsize, nc);
		    	getData(X, Y, kernel, LocX, LocY, LocW, rowrange, colrange, totnum);
		    	VarW = NTau(LocW, LocX, LocY);
		    	if (VarW <= 0)
		    		result[i][j] = 0;
		    	else
		    	{
		    		WtKendallTau kendalltau = new WtKendallTau(LocX, LocY, LocW);
		    		tau = kendalltau.calculate();
			    	result[i][j] = tau * Math.sqrt(VarW) * 1.5;
		    	}
		    }
		  }
		
		return result;
	}
	
	private void getData(double[][] x, double[][] y, double[][] w, double[] sx, double[] sy, double[] sw, int[] rowrange, int[] colrange, int totnum) {
		int kernelk = rowrange[0] - rowrange[2] + rowrange[3];
		int kernell;
		int index = 0;
		
		for (int k = rowrange[0]; k <= rowrange[1]; k++)
	    {
			kernell = colrange[0] - colrange[2] + colrange[3];
	        for (int l = colrange[0]; l <= colrange[1]; l++)
	        {
	        	sx[index] = x[k][l];
	            sy[index] = y[k][l];
	            sw[index] = w[kernelk][kernell];
	            kernell++;
	            index++;
	        }
	        kernelk++;
	      }
		while(index < totnum)
	      {
	        sx[index] = 0;
	        sy[index] = 0;
	        sw[index] = 0;
	        index++;
	      }
	}
	
	private int[] getRange(int location, int radius, int boundary) {
		int[] range = new int[4];
		range[0] = location - radius;
	    if (range[0] < 0)
	    	range[0] = 0;
	    range[1] = location + radius;
	    if (range[1] >= boundary)
	    	range[1] = boundary - 1;
	    range[2] = location;
	    range[3] = radius;
		
	    return range;
	}
	
//	private double varTau(double[] w, double[] x, double[] y) {
//		double sumW=0;
//	    double  sumsqrtW=0;
//	    double Amplifer=10000;
//	    for (int index = 0; index < w.length; index++)
//	    {
//	        w[index] = w[index];
//	        if (x[index]<this.ThreX || y[index]<this.ThreY )
//	        	w[index]=0;
////	    	w[index] = w[index] * x[index] * x[index] * y[index] * y[index];
////	    	w[index] = w[index] / (1+Math.exp(-(x[index]-ThreX)*Amplifer)) / (1+Math.exp(-(y[index]-ThreY)*Amplifer));
//	        sumW += w[index];
//	        sumsqrtW += w[index]*w[index];
//	    }
//	    double  VarW;
//	    if (sumsqrtW == 0)
//	    	VarW = 0;
//	    else
//	    	VarW = sumW * sumW / sumsqrtW;
//	    return VarW;
//	}
	
	private double NTau(double[] w, double[] x, double[] y) {
		double sumW=0;
	    double sumsqrtW=0;
	    double tempW;
	    
	    for (int index = 0; index < w.length; index++)
		    {
		        if (x[index]<this.ThreX || y[index]<this.ThreY )
		        	w[index]=0;
		        tempW = w[index];
		        sumW += tempW;
		        tempW = tempW * w[index];
		        sumsqrtW += tempW;
		    }
	    double  NW;
	    double Denomi = sumW * sumW;
	    if (Denomi <= 0)
	    {
	    	NW = 0;
	    }
	    else
	    {
	    	NW = Denomi / sumsqrtW;
    	}
	    return NW;
	}
	
	private double[][] kernelGenerate(int size) {
		int L = size * 2 + 1;
		double[][] kernel = new double[L][L];
		int center = size;
		double temp;
		
		for (int i = 0;i <= size; i++)
		{
		    for (int j = 0;j <= size; j++)
		    {
		      temp = Math.sqrt(i*i+j*j)/size;
		      if (temp>1)
		        temp=0;
		      else
		        temp=1-temp;
		      kernel[center+i][center+j] = temp;
		      kernel[center-i][center+j] = temp;
		      kernel[center+i][center-j] = temp;
		      kernel[center-i][center-j] = temp;
		    }
		}
		return kernel;
	}

}
