package de.viadee.xai.anchor.adapter.image;

import ij.ImagePlus;
import sc.fiji.CMP_BIA.segmentation.superpixels.jSLIC;

import java.util.stream.Stream;

/**
 * SLIC algorithm usable to extract pixel groups, called superpixels.
 *
 * These may in return be used to perturb images.
 *
 * For more information, see <a href=http://www.kev-smith.com/papers/SLIC_Superpixels.pdf>here.</a>
 */
public enum SLICSuperpixelation {;

    /**
     * Extracts superpixels out of an image.
     *
     * @param representation the image
     * @param grid the grid size
     * @param reg the regularity
     * @return the labelled image instance
     */
    public static ImageInstance extractSuperpixels(ImageRepresentation representation, int grid, float reg) {
        final ImagePlus imagej = new ImagePlus(null, representation.asBufferedImage());
        final jSLIC sp = new jSLIC(imagej);
        sp.process(grid, reg);
        final int[][] data = sp.getSegmentation().getData();
        if (data.length != representation.getWidth())
            throw new RuntimeException();
        if (Stream.of(data).anyMatch(a -> a.length != representation.getWidth()))
            throw new RuntimeException();
        final int[] labels = new int[representation.getHeight() * representation.getWidth()];
        for (int i = 0; i < data.length; i++) {
            for (int j = 0; j < data[i].length; j++) {
                labels[i + j * representation.getWidth()] = data[i][j];
            }
        }
        return new ImageInstance(representation, labels);
    }
}
