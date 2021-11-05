#pragma once

#include "Image.h"

class SeamCarver
{
    using Seam = std::vector<size_t>;

public:
    SeamCarver(Image image);

    /**
     * Returns current image
     */
    const Image& GetImage() const;

    /**
     * Gets current image width
     */
    size_t GetImageWidth() const;

    /**
     * Gets current image height
     */
    size_t GetImageHeight() const;

    /**
     * Returns pixel energy
     * @param columnId column index (x)
     * @param rowId row index (y)
     */
    double GetPixelEnergy(size_t columnId, size_t rowId) const;

    /**
     * Returns sequence of pixel row indexes (y)
     * (x indexes are [0:W-1])
     */
    Seam FindHorizontalSeam() const;

    /**
     * Returns sequence of pixel column indexes (x)
     * (y indexes are [0:H-1])
     */
    Seam FindVerticalSeam() const;

    /**
     * Removes sequence of pixels from the image
     */
    void RemoveHorizontalSeam(const Seam& seam);

    /**
     * Removes sequence of pixes from the image
     */
    void RemoveVerticalSeam(const Seam& seam);

private:
    Image m_image;
    std::vector<std::vector<double>> energy;
    void all_energy();
    double energy_by_x(size_t columnId, size_t rowId);
    double energy_by_y(size_t columnId, size_t rowId);
    void swap_pixel(Image::Pixel first, Image::Pixel second);
    std::pair<double, std::pair<size_t, size_t>> get_min_energy_horizont_seam(double now_energy, double left, double right, double up, size_t i, size_t j) const;
    std::pair<double, std::pair<size_t, size_t>> get_min_energy_vertical_seam(double now_energy, double up, double down, double left, size_t i, size_t j) const;
    std::vector<size_t> get_horizont_seam(std::vector<std::vector<std::pair<double, std::pair<size_t, size_t>>>>& horiz_energy, size_t Width, size_t Height) const;
    std::vector<size_t> get_vertical_seam(std::vector<std::vector<std::pair<double, std::pair<size_t, size_t>>>>& vert_energy, size_t Width, size_t Height) const;
};