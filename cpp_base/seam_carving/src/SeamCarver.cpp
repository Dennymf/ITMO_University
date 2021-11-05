#include "SeamCarver.h"
#include <algorithm>
#include <cmath>

SeamCarver::SeamCarver(Image image)
    : m_image(std::move(image))
{
    all_energy();
}

void SeamCarver::all_energy()
{
    energy.resize(GetImageWidth(), std::vector<double> (GetImageHeight()));

    for(size_t i = 0; i < energy.size(); i++) {
        for(size_t j = 0; j < energy[i].size(); j++){
            energy[i][j] = std::sqrt(energy_by_x(i, j) + energy_by_y(i, j));
        }
    }
}

double SeamCarver::energy_by_x(size_t columnId, size_t rowId)
{
    size_t column_up = (columnId == GetImageWidth() - 1 ? 0 : columnId + 1);
    size_t column_down = (columnId == 0 ? GetImageWidth() - 1 : columnId - 1);
    Image::Pixel pixel_up = GetImage().GetPixel(column_up, rowId);
    Image::Pixel pixel_down = GetImage().GetPixel(column_down, rowId);

    return std::pow(pixel_up.m_red - pixel_down.m_red, 2) +
           std::pow(pixel_up.m_green - pixel_down.m_green, 2) +
           std::pow(pixel_up.m_blue - pixel_down.m_blue, 2);
}

double SeamCarver::energy_by_y(size_t columnId, size_t rowId)
{
    size_t row_right = (rowId == GetImageHeight() - 1 ? 0 : rowId + 1);
    size_t row_left = (rowId == 0 ? GetImageHeight() - 1 : rowId - 1);
    Image::Pixel pixel_right = GetImage().GetPixel(columnId, row_right);
    Image::Pixel pixel_left = GetImage().GetPixel(columnId, row_left);

    return std::pow(pixel_right.m_red - pixel_left.m_red, 2) +
           std::pow(pixel_right.m_green - pixel_left.m_green, 2) +
           std::pow(pixel_right.m_blue - pixel_left.m_blue, 2);
}

const Image& SeamCarver::GetImage() const
{
    return m_image;
}

size_t SeamCarver::GetImageWidth() const
{
    return GetImage().m_table.size();
}

size_t SeamCarver::GetImageHeight() const
{
    return GetImage().m_table[0].size();
}

double SeamCarver::GetPixelEnergy(size_t columnId, size_t rowId) const
{
    return energy[columnId][rowId];
}

std::pair<double, std::pair<size_t, size_t>> SeamCarver::get_min_energy_horizont_seam(double now_energy, double left, double right, double up,
                                                                                        size_t i, size_t j) const
{
    double new_left_energy = now_energy + left;
    double new_right_energy = now_energy + right;
    double new_up_energy = now_energy + up;

    if(new_left_energy <= new_right_energy && new_left_energy <= new_up_energy)
    {
        return {new_left_energy, {i - 1, j - 1}};
    }
    else if(new_up_energy <= new_left_energy && new_up_energy <= new_right_energy)
    {
        return {new_up_energy, {i - 1, j }};
    }
    else
    {
        return {new_right_energy, {i - 1, j + 1}};
    }
};

std::vector<size_t> SeamCarver::get_horizont_seam(std::vector<std::vector<std::pair<double, std::pair<size_t, size_t>>>>& horiz_energy, size_t Width, size_t Height) const
{
    Seam res = Seam();
    std::pair<double, size_t> min_and_index = {horiz_energy.back()[0].first , 0};
    std::pair<size_t, size_t> parent = horiz_energy.back()[0].second;

    for(size_t i = 1; i < Height; i++){
        if(horiz_energy.back()[i].first < min_and_index.first)
        {
            min_and_index = {horiz_energy.back()[i].first, i};
            parent = horiz_energy.back()[i].second;
        }
    }

    res.push_back(min_and_index.second);

    while(res.size() < Width) {
        res.push_back(parent.second);
        parent = horiz_energy[parent.first][parent.second].second;
    }

    std::reverse(res.begin(), res.end());

    return res;
}

SeamCarver::Seam SeamCarver::FindHorizontalSeam() const
{
    std::vector<std::vector<std::pair<double, std::pair<size_t, size_t>>>> energy_horizont_seam;

    size_t Height = GetImageHeight();
    size_t Width = GetImageWidth();

    energy_horizont_seam.resize(Width,std::vector<std::pair<double, std::pair<size_t, size_t>>>(Height));

    for(size_t j = 0; j < Height; j++){
        energy_horizont_seam[0][j].first = energy[0][j];
    }

    for(size_t i = 1; i < Width; i++){
        for(size_t j = 0; j < Height; j++){
            energy_horizont_seam[i][j] = get_min_energy_horizont_seam(energy[i][j],
                                                                      j > 0 ? energy_horizont_seam[i - 1][j - 1].first : 1e8,
                                                                      j < Height - 1 ? energy_horizont_seam[i - 1][j + 1].first : 1e8,
                                                                      energy_horizont_seam[i - 1][j].first,
                                                                      i, j);
        }
    }

    Seam horizont_seam = get_horizont_seam(energy_horizont_seam, Width, Height);

    return horizont_seam;
}

std::pair<double, std::pair<size_t, size_t>> SeamCarver::get_min_energy_vertical_seam(double now_energy, double up, double down, double left,
                                                                                      size_t i, size_t j) const
{
    double new_up_energy = now_energy + up;
    double new_down_energy = now_energy + down;
    double new_left_energy = now_energy + left;

    if(new_up_energy <= new_left_energy && new_up_energy <= new_down_energy)
    {
        return {new_up_energy, {i - 1, j - 1}};
    }
    else if(new_left_energy <= new_up_energy && new_left_energy <= new_down_energy)
    {
        return {new_left_energy, {i, j - 1}};
    }
    else
    {
        return {new_down_energy, {i + 1, j - 1}};
    }
};

std::vector<size_t> SeamCarver::get_vertical_seam(std::vector<std::vector<std::pair<double, std::pair<size_t, size_t>>>>& vert_energy, size_t Width, size_t Height) const
{
    Seam res = Seam();
    std::pair<double, size_t> min_and_index = {vert_energy[0].back().first , 0};
    std::pair<size_t, size_t> parent = vert_energy[0].back().second;

    for(size_t i = 1; i < Width; i++){
        if(vert_energy[i].back().first < min_and_index.first)
        {
            min_and_index = {vert_energy[i].back().first, i};
            parent = vert_energy[i].back().second;
        }
    }

    res.push_back(min_and_index.second);

    while(res.size() < Height) {
        res.push_back(parent.first);
        parent = vert_energy[parent.first][parent.second].second;
    }

    std::reverse(res.begin(), res.end());

    return res;
}

SeamCarver::Seam SeamCarver::FindVerticalSeam() const
{
    std::vector<std::vector<std::pair<double, std::pair<size_t, size_t>>>> energy_vertical_seam;

    size_t Height = GetImageHeight();
    size_t Width = GetImageWidth();

    energy_vertical_seam.resize(Width, std::vector<std::pair<double, std::pair<size_t, size_t>>>(Height));

    for(size_t i = 0; i < Width; i++){
        energy_vertical_seam[i][0].first = energy[i][0];
    }

    for(size_t j = 1; j < Height; j++) {
        for (size_t i = 0; i < Width; i++) {
            energy_vertical_seam[i][j] = get_min_energy_vertical_seam(energy[i][j],
                                                                      i > 0 ? energy_vertical_seam[i - 1][j - 1].first : 1e8,
                                                                      i < Width - 1 ? energy_vertical_seam[i + 1][j - 1].first : 1e8,
                                                                      energy_vertical_seam[i][j - 1].first,
                                                                      i, j);
        }
    }

    Seam vertical_seam = get_vertical_seam(energy_vertical_seam, Width, Height);

    return vertical_seam;
}

void SeamCarver::swap_pixel(Image::Pixel first, Image::Pixel second)
{
    Image::Pixel temp = first;
    first = second;
    second = temp;
}

void SeamCarver::RemoveHorizontalSeam(const Seam& seam)
{
    for(size_t i = 0; i < seam.size(); i++){
        for(size_t j = GetImageHeight() - 1; j > seam[i]; j--){
            Image::Pixel first = GetImage().GetPixel(i, j);
            Image::Pixel second = GetImage().GetPixel(i, seam[i]);
            swap_pixel(first, second);
        }
    }

    for(size_t i = 0; i < GetImageWidth(); i++){
        m_image.m_table[i].pop_back();
    }
}

void SeamCarver::RemoveVerticalSeam(const Seam& seam)
{
    for(size_t i = 0; i < seam.size(); i++){
        for(size_t j = GetImageWidth() - 1; j > seam[i]; j--){
            Image::Pixel first = GetImage().GetPixel(j, i);
            Image::Pixel second = GetImage().GetPixel(seam[i], i);
            swap_pixel(first, second);
        }
    }

    m_image.m_table.pop_back();
}
