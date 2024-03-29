# modification of https://gist.github.com/brendano/39760
# automatically obtains data from the web
# creates two data frames, test and train
# labels are stored in the y variables of each data frame
# can easily train many models using formula `y ~ .` syntax

# download data from http://yann.lecun.com/exdb/mnist/
# download.file("http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz",
              # "train-images-idx3-ubyte.gz")
# download.file("http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz",
              # "train-labels-idx1-ubyte.gz")
# download.file("http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz",
              # "t10k-images-idx3-ubyte.gz")
# download.file("http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz",
              # "t10k-labels-idx1-ubyte.gz")

# gunzip the files
# R.utils::gunzip("train-images-idx3-ubyte.gz")
# R.utils::gunzip("train-labels-idx1-ubyte.gz")
# R.utils::gunzip("t10k-images-idx3-ubyte.gz")
# R.utils::gunzip("t10k-labels-idx1-ubyte.gz")

# helper function for visualization
show_digit = function(arr784, col = gray(12:1 / 12), ...) {
  image(matrix(as.matrix(arr784[-785]), nrow = 28)[, 28:1], col = col, ...)
}

show_digit_emnist = function(arr784, col = gray(12:1 / 12), ...) {
  image(t(matrix(as.matrix(arr784[-785]), nrow = 28))[, 28:1], col = col, ...)
}

# load image files
load_image_file = function(filename) {
  ret = list()
  f = file(filename, 'rb')
  readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  n    = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  nrow = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  ncol = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  x = readBin(f, 'integer', n = n * nrow * ncol, size = 1, signed = FALSE)
  close(f)
  data.frame(matrix(x, ncol = nrow * ncol, byrow = TRUE))
}

# load label files
load_label_file = function(filename) {
  f = file(filename, 'rb')
  readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  n = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  y = readBin(f, 'integer', n = n, size = 1, signed = FALSE)
  close(f)
  y
}

# dir <- "C:/Users/AAAAAAr/Documents/Divers/Réseaux de neurones/MNIST/"
# setwd(dir)
# load images
# train = load_image_file("C:/Users/AAAAAAr/Documents/Divers/Réseaux de neurones/MNIST/train-images.idx3-ubyte")
# test  = load_image_file("C:/Users/AAAAAAr/Documents/Divers/Réseaux de neurones/MNIST/t10k-images.idx3-ubyte")

# head(train)

# load labels
# train$y = load_label_file("C:/Users/AAAAAAr/Documents/Divers/Réseaux de neurones/MNIST/train-labels.idx1-ubyte")
# test$y  = load_label_file("C:/Users/AAAAAAr/Documents/Divers/Réseaux de neurones/MNIST/t10k-labels.idx1-ubyte")

# head(test)

# view test image
# show_digit(train[10000, ])


# dir <- "C:/Users/AAAAAAr/Documents/Divers/Réseaux de neurones/MNIST/"
# setwd(dir)
# load images
train = load_image_file("C:/Users/BBBBB/Documents/NeuralNetworks/gzip/emnist-letters-train-images-idx3-ubyte")
test  = load_image_file("C:/Users/BBBBB/Documents/NeuralNetworks/gzip/emnist-letters-test-images-idx3-ubyte")

head(train)

# load labels
train$y = load_label_file("C:/Users/BBBBB/Documents/NeuralNetworks/gzip/emnist-letters-train-labels-idx1-ubyte")
test$y  = load_label_file("C:/Users/BBBBB/Documents/NeuralNetworks/gzip/emnist-letters-test-labels-idx1-ubyte")

# view test image
show_digit_emnist(train[10000, ])

summary(as.factor(test[,785]))

# testing classification on subset of training data
# fit = randomForest::randomForest(y ~ ., data = train[1:1000, ])
# fit$confusion
# test_pred = predict(fit, test)
# mean(test_pred == test$y)
# table(predicted = test_pred, actual = test$y)