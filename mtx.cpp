#include <iostream>
#include <random>
#include <vector>
#include <deque>
#include <cmath>
#include <stdexcept>

namespace Math
{

static const std::uint32_t GenerateRandom(const std::uint32_t min, const std::uint32_t max)
{
    std::random_device device;
    std::mt19937 generator(device());
    std::uniform_int_distribution<std::uint32_t> distribution(min, max);
    return distribution(generator);
}

static const std::vector<unsigned long int> GenerateRandomBytes(const std::size_t min,
                                                                const std::size_t max)
{
    std::vector<unsigned long int> randomBytes;
    for (short int count = 0; count < 100; ++count)
    {
        randomBytes.push_back(GenerateRandom(min, max));
    }
    return randomBytes;
}

/**
 * @brief Function to generate random characters
 */
__attribute__((hot)) static const std::vector<char> GenerateRandomChars(
    const unsigned short int threshold)
{
    std::random_device device;
    std::mt19937 generator(device());
    std::uniform_int_distribution<> distribution(65, 80);
    std::vector<char> charVector(threshold);
    for (std::size_t count = 0; count < threshold; ++count)
    {
        charVector[count] = static_cast<char>(distribution(generator));
    }
    return charVector;
}

static std::vector<unsigned long int> ComputePrimes(const std::size_t n)
{
    std::vector<unsigned long int> primes;
    primes.push_back(2);
    try
    {
        for (unsigned long int number = 3; number <= n; number += 2)
        {
            bool isPrime = true;
            for (unsigned long int i = 3; i <= std::sqrt(number); i += 2)
            {
                if (number % i == 0)
                {
                    isPrime = false;
                    break;
                }
            }
            if (isPrime)
            {
                primes.push_back(number);
            }
        }
    }
    catch (const std::exception &e)
    {
        std::cerr << "Error: " << e.what() << "\n";
    }
    return primes;
}

template <int Rows, int Columns> struct MatrixDimensions
{
    std::uint32_t rows{Rows};
    std::uint32_t columns{Columns};

    MatrixDimensions() = default;
    MatrixDimensions(std::uint32_t rows, std::uint32_t columns) : rows(rows), columns(columns)
    {
    }
};

template <typename T> struct IsContainer : std::false_type
{
};

template <typename T, typename Alloc> struct IsContainer<std::vector<T, Alloc>> : std::true_type
{
};

template <typename T, typename Alloc> struct IsContainer<std::deque<T, Alloc>> : std::true_type
{
};

template <typename T, std::uint32_t Rows, std::uint32_t Columns> struct Matrix
{
    std::vector<std::deque<T>> data{};
    MatrixDimensions<Rows, Columns> dimensions;

    explicit Matrix() = default;
    Matrix(std::vector<std::deque<T>> vec, MatrixDimensions<Rows, Columns> dims)
        : data(vec), dimensions(dims)
    {
    }

    template <typename OtherType, std::uint32_t OtherRows, std::uint32_t OtherColumns>
    Matrix<OtherType, OtherRows, OtherColumns> &operator=(
        const Matrix<OtherType, OtherRows, OtherColumns> &other)
    {
        std::cout << "Copy assignment operator\n";
        data = other.data;
        dimensions = other.dimensions;
        return *this;
    }

    const bool operator==(const Matrix<T, Rows, Columns> &other) const
    {
        std::cout << "Equality comparison\n";
        if (dimensions.rows == other.dimensions.rows &&
            dimensions.columns == other.dimensions.columns)
        {
            for (std::uint32_t i = 0; i < dimensions.rows; ++i)
            {
                for (std::uint32_t j = 0; j < dimensions.columns; ++j)
                {
                    if (data[i][j] != other.data[i][j])
                    {
                        return false;
                    }
                }
            }
            return true;
        }
        return false;
    }

    void DistributeMono(const std::vector<T> &values)
    {
        std::uint16_t rowIndex = 0, columnIndex = 0;
        Clear();
        for (std::uint32_t j = 0; j < values.size(); ++j)
        {
            if (rowIndex == dimensions.rows)
            {
                break;
            }
            else if (columnIndex == dimensions.columns)
            {
                columnIndex = 0;
                ++rowIndex;
                --j; // Re-evaluate the same index
                continue;
            }
            data[rowIndex][columnIndex] = values[j];
            ++columnIndex;
        }
    }

    void DistributePoly(const std::vector<std::deque<T>> &valueSets)
    {
        Clear();
        if (valueSets.size() <= dimensions.rows)
        {
            for (std::uint32_t i = 0; i < valueSets.size(); ++i)
            {
                if (!valueSets[i].empty() && valueSets[i].size() <= dimensions.columns)
                {
                    data[i] = valueSets[i];
                }
            }
        }
    }

    template <typename SizeType>
    void SetDimensions(const SizeType newRows, const SizeType newColumns)
    {
        dimensions.rows = newRows;
        dimensions.columns = newColumns;
        data.resize(newRows);
        for (std::uint32_t i = 0; i < newRows; ++i)
        {
            data[i].resize(newColumns);
        }
    }

    void Clear()
    {
        data.resize(dimensions.rows);
        for (std::uint32_t i = 0; i < dimensions.rows; ++i)
        {
            data[i].resize(dimensions.columns);
        }
    }

    void Transpose()
    {
        std::vector<std::deque<T>> tempData;
        tempData.swap(data);
        data.clear();
        SetDimensions<T>(dimensions.columns, dimensions.rows);
        for (std::uint32_t i = 0; i < dimensions.rows; ++i)
        {
            for (std::uint32_t j = 0; j < dimensions.columns; ++j)
            {
                data[i][j] = tempData[j][i];
            }
        }
        tempData.clear();
    }

    template <typename OtherType, std::uint32_t OtherRows, std::uint32_t OtherColumns>
    Matrix<OtherType, OtherRows, OtherColumns> operator+(
        const Matrix<OtherType, OtherRows, OtherColumns> &other) const
    {
        Matrix<OtherType, OtherRows, OtherColumns> result;
        try
        {
            if (other.dimensions.rows != dimensions.rows ||
                other.dimensions.columns != dimensions.columns)
            {
                throw std::runtime_error("Matrices do not match in dimensions!");
            }
            result.Clear();                 // Initialize values to 0
            result.dimensions = dimensions; // Copy dimensions

            for (std::size_t i = 0; i < dimensions.rows; ++i)
            {
                for (std::size_t j = 0; j < dimensions.columns; ++j)
                {
                    result.data[i][j] = data[i][j] + other.data[i][j];
                }
            }
        }
        catch (const std::exception &e)
        {
            std::cerr << "Error: " << e.what() << "\n";
        }
        return result;
    }

    const std::size_t Size() const
    {
        return dimensions.rows * dimensions.columns;
    }
};

} // namespace Math

int main(int argc, char **argv)
{
    try
    {
        // ---- Manual matrix distribution
        // Initialize new matrix structure
        Math::Matrix<std::uint32_t, 2u, 3u> intMatrix;
        intMatrix.data.resize(intMatrix.dimensions.rows); // Resize blocks

        // Populate...
        intMatrix.data[0] = {3, 6, 5};
        intMatrix.data[1] = {7, 9, 2};

        std::cout << "Matrix:\n";
        // Distribute from single set
        intMatrix.DistributeMono({1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6});

        // Print first matrix
        for (std::uint32_t x = 0; x < intMatrix.dimensions.rows; ++x)
        {
            for (std::uint32_t y = 0; y < intMatrix.dimensions.columns; ++y)
            {
                std::cout << intMatrix.data[x][y] << " ";
            }
            std::cout << "\n";
        }

        intMatrix.SetDimensions<std::uint32_t>(5, 6); // Resize to 5*6 matrix

        std::cout << "\nMatrix after resize to 5*6:\n";
        // Distribute from multi sets
        intMatrix.DistributePoly({{23, 54, 76, 34, 44, 77},
                                  {27, 38, 29, 10, 11, 12},
                                  {13, 14, 15, 16, 17, 18},
                                  {19, 20, 21, 22, 23, 24},
                                  {25, 26, 27, 28, 29, 30}});

        for (std::uint32_t x = 0; x < intMatrix.dimensions.rows; ++x)
        {
            for (std::uint32_t y = 0; y < intMatrix.dimensions.columns; ++y)
            {
                std::cout << intMatrix.data[x][y] << " ";
            }
            std::cout << "\n";
        }

        intMatrix.SetDimensions<std::uint32_t>(9, 15); // Expand to 9*15 matrix

        // ---- Computed matrix distribution
        std::vector<unsigned long> primeCandidates = Math::ComputePrimes(1000);
        std::vector<unsigned long> primes;
        primes.reserve(intMatrix.dimensions.rows * intMatrix.dimensions.columns);

        for (std::size_t d = 0; d < primeCandidates.size(); ++d)
        {
            if (primeCandidates[d] > 100 && primeCandidates[d] < 999)
            {
                primes.push_back(primeCandidates[d]);
            }
        }

        // Populate matrix structure with prime numbers
        std::size_t primeIndex = 0;
        for (std::uint32_t row = 0; row < intMatrix.dimensions.rows; ++row)
        {
            for (std::uint32_t col = 0; col < intMatrix.dimensions.columns; ++col)
            {
                if (primeIndex >= primes.size())
                {
                    break;
                }
                intMatrix.data[row][col] = primes[primeIndex++];
            }
            if (primeIndex >= primes.size())
            {
                break;
            }
        }

        std::cout << "\nMatrix with Prime Numbers:\n";
        // Print prime numbers matrix
        for (std::uint32_t x = 0; x < intMatrix.dimensions.rows; ++x)
        {
            for (std::uint32_t y = 0; y < intMatrix.dimensions.columns; ++y)
            {
                std::cout << intMatrix.data[x][y] << " ";
            }
            std::cout << "\n";
        }
        std::cout << "\n\n";

        // Execute transposition on matrix with primes
        intMatrix.Transpose();

        std::cout << "\nMatrix with Primes after Transposition:\n";
        // Print after transposition
        for (std::uint32_t x = 0; x < intMatrix.dimensions.rows; ++x)
        {
            for (std::uint32_t y = 0; y < intMatrix.dimensions.columns; ++y)
            {
                std::cout << intMatrix.data[x][y] << " ";
            }
            std::cout << "\n";
        }
        std::cout << "Prime field matrix size: " << intMatrix.Size() << "\n";

        // Populate matrix structure with random numbers (100-999)
        for (std::uint32_t row = 0; row < intMatrix.dimensions.rows; ++row)
        {
            for (std::uint32_t col = 0; col < intMatrix.dimensions.columns; ++col)
            {
                intMatrix.data[row][col] = Math::GenerateRandom(100, 999);
            }
        }

        std::cout << "\nMatrix with Random Numbers:\n";
        // Print random numbers
        for (std::uint32_t x = 0; x < intMatrix.dimensions.rows; ++x)
        {
            for (std::uint32_t y = 0; y < intMatrix.dimensions.columns; ++y)
            {
                std::cout << intMatrix.data[x][y] << " ";
            }
            std::cout << "\n";
        }

        // Create additional matrices for demonstration
        Math::Matrix<std::uint32_t, 2, 3> matrixA, matrixB, matrixC, finalMatrix;
        matrixA.DistributeMono({1, 2, 3, 4, 5, 6});
        matrixB = matrixA;
        matrixC = matrixB;
        finalMatrix.DistributeMono({1, 2, 3, 4, 5, 6});

        std::cout << "Dimensions: matrixB(" << matrixB.dimensions.rows << "*"
                  << matrixB.dimensions.columns << "), matrixC(" << matrixC.dimensions.rows << "*"
                  << matrixC.dimensions.columns << "), finalMatrix(" << finalMatrix.dimensions.rows
                  << "*" << finalMatrix.dimensions.columns << ")\n";

        std::cout << "matrixA [0][0] = " << matrixA.data[0][0] << "\n";
        std::cout << "matrixB [0][0] = " << matrixB.data[0][0] << "\n";
        std::cout << "matrixC [0][0] = " << matrixC.data[0][0] << "\n";
        std::cout << "finalMatrix [0][0] = " << finalMatrix.data[0][0] << "\n";

        finalMatrix = matrixB + matrixC;
        std::cout << "finalMatrix [0][0] after addition = " << finalMatrix.data[0][0] << "\n";

        finalMatrix = finalMatrix + finalMatrix; // Adding finalMatrix to itself
        std::cout << "Matrix size: " << finalMatrix.Size() << "\n";

       
    }
    catch (const std::exception &e)
    {
        std::cerr << "Global Error: " << e.what() << "\n";
    }
    std::cout << "\n";

    return 0;
}
