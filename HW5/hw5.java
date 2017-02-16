/* Name: Gautam Gupta

   UID: 304282688

   Others With Whom I Discussed Things: Jennifer Tan, David Pu, Kelly Hosokawa

   Other Resources I Consulted: Java 8 Docs

*/

import java.io.*;
import java.util.*;
import java.util.stream.*;
import java.util.concurrent.*;

// a marker for code that you need to implement
class ImplementMe extends RuntimeException {}

// an RGB triple
class RGB {
    public int R, G, B;

    RGB(int r, int g, int b) {
    	R = r;
		G = g;
		B = b;
    }

    public Boolean equals(RGB rhs) {
        return R==rhs.R && 
               G==rhs.G &&
               B==rhs.B;
    }

    public double[] scale(double scalar) {
        double[] ret = new double[3];
        ret[0] = R*scalar;
        ret[1] = G*scalar;
        ret[2] = B*scalar;
        return ret;
    }

    public String toString() { return "(" + R + "," + G + "," + B + ")"; }

}


// an object representing a single PPM image
class PPMImage {
    protected int width, height, maxColorVal;
    protected RGB[] pixels;

    public PPMImage(int w, int h, int m, RGB[] p) {
		width = w;
		height = h;
		maxColorVal = m;
		pixels = p;
    }

    public Boolean equals(PPMImage rhs) {
        if (width != rhs.width ||
                height != rhs.height ||
                maxColorVal != rhs.maxColorVal)
            return false;
        for (int i = 0; i < width*height; i++) {
            if (! pixels[i].equals(rhs.pixels[i])) {
                return false;
            }
        }
        return true;
    }

    // parse a PPM image file named fname and produce a new PPMImage object
    public PPMImage(String fname)
    	throws FileNotFoundException, IOException {
		FileInputStream is = new FileInputStream(fname);
		BufferedReader br = new BufferedReader(new InputStreamReader(is));
		br.readLine(); // read the P6
		String[] dims = br.readLine().split(" "); // read width and height
		int width = Integer.parseInt(dims[0]);
		int height = Integer.parseInt(dims[1]);
		int max = Integer.parseInt(br.readLine()); // read max color value
		br.close();

		is = new FileInputStream(fname);
	    // skip the first three lines
		int newlines = 0;
		while (newlines < 3) {
	    	int b = is.read();
	    	if (b == 10)
				newlines++;
		}

		int MASK = 0xff;
		int numpixels = width * height;
		byte[] bytes = new byte[numpixels * 3];
        is.read(bytes);
		RGB[] pixels = new RGB[numpixels];
		for (int i = 0; i < numpixels; i++) {
	    	int offset = i * 3;
	    	pixels[i] = new RGB(bytes[offset] & MASK,
	    						bytes[offset+1] & MASK,
	    						bytes[offset+2] & MASK);
		}
		is.close();

		this.width = width;
		this.height = height;
		this.maxColorVal = max;
		this.pixels = pixels;
    }

	// write a PPMImage object to a file named fname
    public void toFile(String fname) throws IOException {
		FileOutputStream os = new FileOutputStream(fname);

		String header = "P6\n" + width + " " + height + "\n"
						+ maxColorVal + "\n";
		os.write(header.getBytes());

		int numpixels = width * height;
		byte[] bytes = new byte[numpixels * 3];
		int i = 0;
		for (RGB rgb : pixels) {
	    	bytes[i] = (byte) rgb.R;
	    	bytes[i+1] = (byte) rgb.G;
	    	bytes[i+2] = (byte) rgb.B;
	    	i += 3;
		}
		os.write(bytes);
		os.close();
    }

	// implement using Java 8 Streams
    public PPMImage negate() {
        return
            new PPMImage(width,
                         height,
                         maxColorVal,
                         Arrays.stream(pixels)
                               .parallel()
                               .map(rgb -> new RGB(maxColorVal - rgb.R,
                                                   maxColorVal - rgb.G,
                                                   maxColorVal - rgb.B))
                               .toArray(size -> new RGB[size]));
    }

    private RGB to_greyscale(RGB in) {
        int value = (int)Math.round(.299 * in.R +
                                    .587 * in.G +
                                    .114 * in.B);
        return new RGB(value, value, value);
    }

	// implement using Java 8 Streams
    public PPMImage greyscale() {
        return
            new PPMImage(width,
                         height,
                         maxColorVal,
                         Arrays.stream(pixels)
                               .parallel()
                               .map(rgb -> to_greyscale(rgb))
                               .toArray(size -> new RGB[size]));
    }

    private static class MirrorTask extends RecursiveAction {
        protected RGB[] pixels;
        protected int width, height, colstart, colend;
        private final int SEQ_THRESH = 128;
        public MirrorTask(RGB[] pixels, int width, int height, int colstart, int colend) {
            this.pixels = pixels;
            this.width = width;
            this.height = height;
            this.colstart = colstart;
            this.colend = colend;
        }
        public void compute() {
            if (colend - colstart < SEQ_THRESH) {
                for (int i = colstart; i < colend; i++) {
                    for (int j = 0; j < height; j++) {
                        int left_idx = j*width + i;
                        int right_idx = j*width + (width - i - 1);
                        RGB temp = pixels[right_idx];
                        pixels[right_idx] = pixels[left_idx];
                        pixels[left_idx] = temp;
                    }
                }
                return;
            }
            int colmid = (colstart + colend) / 2;
            MirrorTask left = new MirrorTask(pixels, width, height, colstart, colmid);
            MirrorTask right = new MirrorTask(pixels, width, height, colmid, colend);
            right.fork();
            left.compute();
            right.join();
        }
    }

	// implement using Java's Fork/Join library
    public PPMImage mirrorImage() {
        PPMImage mirrored = new PPMImage(width,
                                         height,
                                         maxColorVal,
                                         pixels.clone());
        MirrorTask t = new MirrorTask(mirrored.pixels, width, height, 0, width/2);
        t.compute();
        return mirrored;
    }

	// implement using Java 8 Streams
    public PPMImage mirrorImage2() {
        return new PPMImage(
            width,
            height,
            maxColorVal,
            IntStream.range(0, width*height)
                        .parallel()
                        .map(i ->
                            ((i % width) < (width/2) ?
                            i - 1 + 2 * ((width / 2) - (i % width)) :
                            i - 1 - 2 * ((i % width) - (width / 2))))
                        .mapToObj(i -> pixels[i])
                        .toArray(size -> new RGB[size]));
    }

    private static class GaussianTask extends RecursiveAction {
        protected RGB[] in, out;
        protected int start, end, width, height, radius;
        protected double[][] filter;
        protected int SEQ_THRESH;
        public GaussianTask(int radius, double[][] filter, RGB[] in, RGB[] out, int width, int height, int start, int end) {
            this.in = in;
            this.out = out;
            this.start = start;
            this.end = end;
            this.height = height;
            this.width = width;
            this.radius = radius;
            this.filter = filter;
            this.SEQ_THRESH = width * height / 32;
        }
        private int getIndex(int original, int rowOff, int colOff) {
            int ret = original;
            if (colOff < 0) {
                int clampedOffset = -1 * (original % width);
                ret += Math.max(clampedOffset, colOff);
            }
            else if (colOff > 0) {
                int clampedOffset = width - (original % width) - 1;
                ret += Math.min(clampedOffset, colOff);
            }
            if (rowOff < 0) {
                ret += rowOff * width;
                while (ret < 0)
                    ret += width;
            }
            else if (rowOff > 0) {
                ret += rowOff * width;
                while (ret >= height*width)
                    ret -= width;
            }
            return ret;
        }
        private static void addRGB(double[] source, double[] added) {
            for (int i = 0; i < 3; i++) {
                source[i] += added[i];
            }
        }
        public void compute() {
            if (end - start < SEQ_THRESH) {
                int radmin = 0 - radius;
                int radmax = 0 + radius;
                for (int iter = start; iter < end; iter++) {
                    double[] filteredPixel = {0.0, 0.0, 0.0};
                    for (int i = radmin; i <= radmax; i++) {
                        for (int j = radmin; j <= radmax; j++) {
                            addRGB(filteredPixel,
                                    in[getIndex(iter, i, j)].scale(
                                        filter[i+radius][j+radius])
                                  );
                        } // for j
                    } // for i
                    out[iter] = new RGB((int)Math.round(filteredPixel[0]),
                                        (int)Math.round(filteredPixel[1]),
                                        (int)Math.round(filteredPixel[2]));
                } // for iter
                return;
            } // if
            else {
                int mid = (end + start) / 2;
                GaussianTask left = new GaussianTask(radius, filter, in, out, width, height, start, mid);
                GaussianTask right = new GaussianTask(radius, filter, in, out, width, height, mid, end);
                left.fork();
                right.compute();
                left.join();
                return;
            }
        }
    }

	// implement using Java's Fork/Join library
    public PPMImage gaussianBlur(int radius, double sigma) {
        RGB[] out = new RGB[width*height];
        double[][] filter = Gaussian.gaussianFilter(radius, sigma);
        GaussianTask t = new GaussianTask(radius, filter, pixels, out, width, height, 0, width*height);
        t.compute();
        return new PPMImage(width, height, maxColorVal, out);
    }

}

// code for creating a Gaussian filter
class Gaussian {

    protected static double gaussian(int x, int mu, double sigma) {
		return Math.exp( -(Math.pow((x-mu)/sigma,2.0))/2.0 );
    }

    public static double[][] gaussianFilter(int radius, double sigma) {
		int length = 2 * radius + 1;
		double[] hkernel = new double[length];
		for(int i=0; i < length; i++)
	    	hkernel[i] = gaussian(i, radius, sigma);
		double[][] kernel2d = new double[length][length];
		double kernelsum = 0.0;
		for(int i=0; i < length; i++) {
	    	for(int j=0; j < length; j++) {
				double elem = hkernel[i] * hkernel[j];
				kernelsum += elem;
				kernel2d[i][j] = elem;
	    	}
		}
		for(int i=0; i < length; i++) {
	    	for(int j=0; j < length; j++)
				kernel2d[i][j] /= kernelsum;
		}
		return kernel2d;
    }
}

class NegateTest {
    public static void main(String[] args) throws Exception {
        PPMImage im = new PPMImage(args[0]);
        PPMImage im2 = new PPMImage(args[0]);
        im.negate().toFile("negated_" + args[0]);
        assert(im.negate().negate().equals(im));
    }
}
class GreyscaleTest {
    public static void main(String[] args) throws Exception {
        PPMImage im = new PPMImage(args[0]);
        im.greyscale().toFile("greyscale_" + args[0]);
    }
}
class MirrorTest {
    public static void main(String[] args) throws Exception {
        PPMImage im = new PPMImage(args[0]);
        im.mirrorImage().toFile("mirrored_" + args[0]);
        assert(im.mirrorImage().mirrorImage().equals(im));
    }
}
class Mirror2Test {
    public static void main(String[] args) throws Exception {
        PPMImage im = new PPMImage(args[0]);
        im.mirrorImage2().toFile("mirrored2_" + args[0]);
        assert(im.mirrorImage2().mirrorImage2().equals(im));
    }
}
class GaussianTest {
    public static void main(String[] args) throws Exception {
        PPMImage im = new PPMImage(args[0]);
        im.gaussianBlur(30, 40.0).toFile("gaussian_" + args[0]);
    }
}

class AllTest {
    public static void main(String[] args) throws Exception {
        NegateTest.main(args);
        GreyscaleTest.main(args);
        MirrorTest.main(args);
        Mirror2Test.main(args);
        GaussianTest.main(args);
    }
}
