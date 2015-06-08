unit OpenCLIPP;

interface

uses
  CL, CL_GL, CL_Platform;

const
  COpenClipp = 'OpenCLIPP.dll';

type 
  TOcipBool = Byte;   ///< Used to signify which function parameters are boolean

type
  TDataType = (
    dtU8,            /// Unsigned 8-bit integer (unsigned char)
    dtS8,            /// Signed 8-bit integer (char)
    dtU16,           /// Unsigned 16-bit integer (unsigned short)
    dtS16,           /// Signed 16-bit integer (short)
    dtU32,           /// Unsigned 32-bit integer (unsigned int)
    dtS32,           /// Signed 32-bit integer (int)
    dtF32,           /// 32-bit floating point (float)
    dtF64            /// 64-bit floating point (double)
  );  ///< Data type of each channel in the image

  /// The SImage structure is used to tell the library the type and size of images when creating image objects.
  TImage = record
   Width: Cardinal;    ///< Width of the image, in pixels
   Height: Cardinal;   ///< Height of the image, in pixels
   Step: Cardinal;     ///< Nb of bytes between each row
   Channels: Cardinal; ///< Number of channels in the image, allowed values : 1, 2, 3 or 4

   /// EDataType : Lists possible types of data
   DataType: TDataType
  end;

/// Type used as return values of most ocip calls.
/// Successful calls will return CL_SUCCESS (0) while
/// unsuccesful calls will return a negative value.
/// Use ocipGetErrorName() to get the name of the error
  TOcipError = TCL_int;

  TOcipContext = PCL_context; ///< A handle to a context
  TOcipImage = Pointer; // PCL_image;     ///< A handle to an image in the device
  TOcipProgram = PCL_program; ///< A handle to a program


/// Lists the possible interpolation types useable in some primitives
  TOcipInterpolationType = (
   ocipNearestNeighbour,   ///< Chooses the value of the closest pixel - Fastest
   ocipLinear,             ///< Does a bilinear interpolation of the 4 closest pixels
   ocipCubic,              ///< Does a bicubic interpolation of the 16 closest pixels
   ocipLanczos2,           ///< Does 2-lobed Lanczos interpolation using 16 pixels
   ocipLanczos3,           ///< Does 3-lobed Lanczos interpolation using 36 pixels
   ocipSuperSampling,      ///< Samples each pixel of the source for best resize result - for downsizing images only
   ocipBestQuality         ///< Automatically selects the choice that will give the best image quality for the operation
 );


/// Initialization.
/// Initializes OpenCL, creates an execution context, sets the new context as the current context
/// and returns the context handle.
/// The handle must be closed by calling ocipUninitialize when the context (or the whole library) is no longer needed.
/// ocipInitialize() can be called more than once, in that case, each context must be
/// released individually by a call to ocipUninitialize(). Images, Buffers and Programs
/// created from different context can't be mixed (a program can only run
/// with images or buffers that have been created from the same context).
/// \param ContextPtr : The value pointed by ContextPtr will be set to the new context handle
/// \param PreferredPlatform : Can be set to a specific platform (Ex: "Intel") and
///         that platform will be used if available. If the preferred platform is not
///         found or is not specified, the default OpenCL platform will be used.
///         Set to NULL to let OpenCL choose the best computing device available.
/// \param deviceType : can be used to specicy usage of a device type (Ex: CL_DEVICE_TYPE_GPU)
///         See cl_device_type for allowed values
///         Set to CL_DEVICE_TYPE_ALL to let OpenCL choose the best computing device available.
function ocipInitialize(var ContextPtr: TOcipContext;
  const PreferredPlatform: PAnsiChar; deviceType: TCL_device_type): TOcipError; cdecl; external COpenClipp;

/// Uninitialization.
/// Releases the context.
/// \param Context : Handle to the context to uninitialize
function ocipUninitialize(Context: TOcipContext): TOcipError; cdecl; external COpenClipp;


/// Change the current context.
/// Advanced users of the library can use multiple contexts to either :
/// - Use multiple OpenCL devices (multi-GPU or CPU & GPU)
/// - Run multiple operations at a time on the same GPU (to get 100% usage)
/// \param Context : The context to use for the next library calls
function ocipChangeContext(Context: TOcipContext): TOcipError; cdecl; external COpenClipp;


/// Set the Path of .cl files.
/// It is necessary to call this functione before creating any program
/// \param Path : Full path where the .cl files are located
procedure ocipSetCLFilesPath(const Path: PAnsiChar); cdecl; external COpenClipp;


/// Returns the name of the error code
/// \param Error : An OpenCL error code
/// \return the name of the given error code
function ocipGetErrorName(Error: TOcipError): PAnsiChar; cdecl; external COpenClipp;


/// Returns the name of the device used by the given context
/// \param Name : Pointer to a buffer to receive a null terminated string that will contain the device name
/// \param BufferLength : Number of elements in the Name buffer
function ocipGetDeviceName(Name: PAnsiChar; BufferLength: Cardinal): TOcipError; cdecl; external COpenClipp;


/// Waits until all queued operations of this context are done.
/// When this function returns, the device will have finished all operations previously issued on this context.
function ocipFinish: TOcipError; cdecl; external COpenClipp;


// Images

/// Image creation.
/// Allocates memory on the device to store an image like the one in 'Image'
/// If ImageData is not NULL, the pointer value will be saved in the object and
/// then used in these situations :
///   - As source of data when calling ocipSendImage(),
///      meaning memory at that address will be read and then copied in the device memory
///   - As source of data when calling a processing function with this image as source, if
///      this image has never been sent to the device before.
///   - As destination of data when calling ocipReadImage(),
///      meaning memory at that address will be overwritten by the data from the device
///
/// If ImageData is NULL, the image will not be able to be Sent nor Read.
/// Creating an image this way is useful for itermediary results of multi-step calculations or
/// as temporary image for the processing functions that need them.
/// \param ImagePtr : The value pointed to by ImagePtr will be set to the handle of the new image
/// \param Image : A SImage structure describing the image
/// \param ImageData : A pointer to where the image data is located in the main memmory.
///      Can either be NULL (for a device-only image) or point to an image that fits with the description in Image.
/// \param flags : The type of device memory to use, allowed values : CL_MEM_READ_WRITE, CL_MEM_WRITE_ONLY, CL_MEM_READ_ONLY
function ocipCreateImage(var ImagePtr: TOcipImage; Image: TImage; ImageData: Pointer; flags: TCL_mem_flags): TOcipError ; cdecl; external COpenClipp;


/// Sends the image to the device.
/// The image data will referenced by the pointer in the SImage structure given during image creation
/// will be transferred to the device memory.
/// This function executes asyncronously, meaning it will return quickly before the transfer is done.
/// The data pointer given during image creation must remain valid until the transfer is complete.
/// A send operation will be issued automatically when calling a processing function with the image as Source,
/// if the image has not been sent already.
/// Use ocipSendImage to send new image data from the host to the device when the image on the host has changed.
function ocipSendImage(Image: TOcipImage): TOcipError; cdecl; external COpenClipp;

/// Reads the image from the device.
/// The image in the device will be read into the memory pointed to by the pointer in the SImage structure
/// given during image creation.
/// This function will wait until all previous operations, including the read to be complete before returning.
/// So after this function returns, the image on the host will contain the result of the processing operations.
function ocipReadImage(Image: TOcipImage): TOcipError; cdecl; external COpenClipp;

/// Releases an image.
/// Releases the device memory for this image.
/// The TOcipImage handle will no longer be valid.
function ocipReleaseImage(Image: TOcipImage): TOcipError; cdecl; external COpenClipp;


/// Prepare for executing processing operations.
/// ocipPrepareExample() does nothing, it is a place holder for documentation about
/// ocipPrepare* functions that have a single argument.\n
/// These functions will do the following :
///  - Load the .cl file containing the desired program
///  - Initialize an object to hold that program
///  - Build the program for the specified image
///
/// Before calling one of these functions, ocipSetCLFilesPath must have been called with the proper path.\n
/// This description is good for all functions of these forms :\n
/// ocipPrepare*(Image: TOcipImage);\n
/// If ocipPrepare*() is not called before calling the primitive, the operations
/// liste above will be done during the first call to the primitive with that type of image,
/// meaning the first call could take many hundreds of miliseconds.
/// ocipPrepare*() can be called more than once to be ready for images of different types.
/// \param Image : The program will be built (prepared) for that image so that later calls to a processing operation of
///   that category will be fast.
function ocipPrepareExample(Image: TOcipImage): TOcipError; cdecl; external COpenClipp;

/// Prepare for executing processing operations.
/// ocipPrepareExample2() does nothing, it is a place holder for documentation about
/// ocipPrepare* functions that take a ocipProgram * argument.\n
/// These functions will do the following :
///  - Load the .cl file containing the desired program
///  - Initialize an object to hold that program
///  - Build the program for the specified image
///  - Allocate and initialize temprary memory buffers needed for processing the given image
///
/// Before calling one of these functions, ocipSetCLFilesPath must have been called with the proper path.\n
/// This description is good for all functions of this form :\n
/// ocipPrepare*(ocipProgram * ProgramPtr, Image: TOcipImage[, ...]);\n
/// This version of ocipPrepare*() is for primtives that have a ocipProgram argument.
/// These primitives can't be called without a valid program.
/// This is for operations that need temporary buffers or other resources
/// to be allocated in advance. These preprare function will, in addition to building the program,
/// allocate these resources. More than one program handle can be prepared
/// to work with different images.
/// In these cases, ocipReleaseProgram must be called to close the program
/// handle when the program is no longer needed.
/// \param ProgramPtr : A pointer to a variable that will receive the program handle.
/// \param Image : The program will be built (prepared) for the given image.
///                Temporary buffers will also be pre-allocated with the proper size for that image.
function ocipPrepareExample2(out ProgramPtr: TOcipProgram; Image: TOcipImage): TOcipError; cdecl; external COpenClipp;

/// Releases a program.
/// Releases the program, the given program handle will no longer be valid.
function ocipReleaseProgram(&Program: TOcipProgram): TOcipError; cdecl; external COpenClipp;


// Note about processing functions
// Most processing functions are asyncrhonous (non-blocking), meaning 
// they will return quickly before any device computation has been done.
// Many processing functions and Send operations can be issued and then the host
// is free to do other tasks in parralel to the computation.
// Use ocipRead* to wait for completion of all processing operations and transfer
// the result to the host or use ocipFinish() to wait for all queued operations.
// Some program's processing functions are synchronous. If they are, a comment before ocipPrepare*
// will describe their behaviour.
// The processing functions that take a ocipProgram argument  need to be used
// with the proper program, which is the one referenced by
// the handle generated by the ocipPrepare*() function declared above it.
// The first call to a primtive (or the first call with a different type of image)
// will take a long time because the program needs to be prepared for the image.
// To prevent this delay, call ocipPrepare*() with the image beforehand.



// Conversions -----------------------------------------------------------------------------------------
function ocipPrepareConversion(Image: TOcipImage): TOcipError; cdecl; external COpenClipp;  ///< See ocipPrepareExample

/// From any image type to any image type - no value scaling
function ocipConvert(Source, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;

/// From any image type to any image type - automatic value scaling.
/// Scales the input values by the ration of : output range/input range
/// The range is 0,255 for 8u, -128,127 for 8s, ...
/// The range is 0,1 for float
function ocipScale(Source, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;

/// From any image type to any image type with given scaling.
/// Does the conversion Dest = (Source * Ratio) + Offset
function ocipScale2(Source, Dest: TOcipImage; Offset: Integer; Ratio: Single): TOcipError; cdecl; external COpenClipp;

/// Copies an image.
/// Both images must be of the same type.
function ocipCopy(Source, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;


/// Converts a color (4 channel) image to a 1 channel image by averaging the first 3 channels
function ocipToGray(Source, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;

/// Selects 1 channel from a 4 channel image to a 1 channel image - ChannelNo can be from 1 to 4
function ocipSelectChannel(Source, Dest: TOcipImage; ChannelNo: Integer): TOcipError; cdecl; external COpenClipp;

/// Converts a 1 channel image to a 4 channel image - first 3 channels of Dest will be set to the value of the first channel of Source
function ocipToColor(Source, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;


// Arithmetic --------------------------------------------------------------------------------------
function ocipPrepareArithmetic(Image: TOcipImage): TOcipError; cdecl; external COpenClipp;     ///< See ocipPrepareExample
// Between two images
function ocipAdd(Source1, Source2, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;   ///< D = S1 + S2
function ocipAddSquare(Source1, Source2, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;   ///< D = S1 + S2 * S2
function ocipSub(Source1, Source2, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;   ///< D = S1 - S2
function ocipAbsDiff(Source1, Source2, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;   ///< D = abs(S1 - S2)
function ocipMul(Source1, Source2, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;   ///< D = S1 * S2
function ocipDiv(Source1, Source2, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;   ///< D = S1 / S2
function ocipImgMin(Source1, Source2, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;   ///< D = min(S1, S2)
function ocipImgMax(Source1, Source2, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;   ///< D = max(S1, S1)
function ocipImgMean(Source1, Source2, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;   ///< D = (S1 + S2) / 2
function ocipCombine(Source1, Source2, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;   ///< D = sqrt(S1 * S1 + S2 * S2)

// Image and value
function ocipAddC(Source, Dest: TOcipImage; Value: Single): TOcipError; cdecl; external COpenClipp;   ///< D = S + v
function ocipSubC(Source, Dest: TOcipImage; Value: Single): TOcipError; cdecl; external COpenClipp;   ///< D = S - v
function ocipAbsDiffC(Source, Dest: TOcipImage; Value: Single): TOcipError; cdecl; external COpenClipp;   ///< D = abs(S - v)
function ocipMulC(Source, Dest: TOcipImage; Value: Single): TOcipError; cdecl; external COpenClipp;   ///< D = S * v
function ocipDivC(Source, Dest: TOcipImage; Value: Single): TOcipError; cdecl; external COpenClipp;   ///< D = S / v
function ocipRevDivC(Source, Dest: TOcipImage; Value: Single): TOcipError; cdecl; external COpenClipp;   ///< D = v / S
function ocipMinC(Source, Dest: TOcipImage; Value: Single): TOcipError; cdecl; external COpenClipp;   ///< D = min(S, v)
function ocipMaxC(Source, Dest: TOcipImage; Value: Single): TOcipError; cdecl; external COpenClipp;   ///< D = max(S, v)
function ocipMeanC(Source, Dest: TOcipImage; Value: Single): TOcipError; cdecl; external COpenClipp;   ///< D = (S + V) / 2

// Calculation on one image
function ocipAbs(Source, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;   ///< D = abs(S)
function ocipInvert(Source, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;   ///< D = 255 - S
function ocipSqr(Source, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;   ///< D = S * S

// Calculation on one image - float required
function ocipExp(Source, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;   ///< D = exp(S)
function ocipLog(Source, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;   ///< D = log(S)
function ocipSqrt(Source, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;   ///< D = sqrt(S)
function ocipSin(Source, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;   ///< D = sin(S)
function ocipCos(Source, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;   ///< D = cos(S)



// Logic -------------------------------------------------------------------------------------------
function ocipPrepareLogic(Image: TOcipImage): TOcipError; cdecl; external COpenClipp; ///< See ocipPrepareExample
// Bitwise operations - float images not allowed
function ocipAnd(Source1, Source2, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;   ///< D = S1 & S2
function ocipOr(Source1, Source2, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;   ///< D = S1 | S2
function ocipXor(Source1, Source2, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;   ///< D = S1 ^ S2
function ocipAndC(Source, Dest: TOcipImage; Value: Cardinal): TOcipError; cdecl; external COpenClipp;            ///< D = S & v
function ocipOrC(Source, Dest: TOcipImage; Value: Cardinal): TOcipError; cdecl; external COpenClipp;            ///< D = S | v
function ocipXorC(Source, Dest: TOcipImage; Value: Cardinal): TOcipError; cdecl; external COpenClipp;            ///< D = S ^ v
function ocipNot(Source, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;                        ///< D = ~S



// LUT ---------------------------------------------------------------------------------------------
function ocipPrepareLUT(Image: TOcipImage): TOcipError; cdecl; external COpenClipp;   ///< See ocipPrepareExample

/// Performs a LUT operation.
/// levels and values must be arrays of NbValues elements
/// Dest will contain the following transformation :
/// find value v where (S >= levels[v] && S < levels[v + 1])
/// D = values[v]
/// \param levels : Array of size NbValues describing the levels to look at in Source
/// \param values : Array of size NbValues describing the values to use for those levels
function ocipLut(Source, Dest: TOcipImage; Levels, Values: PCardinal; NbValues: Integer): TOcipError; cdecl; external COpenClipp;

/// Performs a linear LUT operation.
/// levels and values must be arrays of NbValues elements
/// Dest will contain the following transformation :
/// find value v where (S >= levels[v] && S < levels[v + 1])
/// ratio = (S - levels[v]) / (levels[v + 1] - levels[v])
/// D = values[v] + (values[v + 1] - values[v]) * ratio
/// \param levels : Array of size NbValues describing the levels to look at in Source
/// \param values : Array of size NbValues describing the values to use for those levels
function ocipLutLinear(Source, Dest: TOcipImage; Levels, Values: PSingle; NbValues: Integer): TOcipError; cdecl; external COpenClipp;

/// Performs a LUT on 8 bit unsigned images.
/// D = values[S]
function ocipBasicLut(Source, Dest: TOcipImage; Values: PByte): TOcipError; cdecl; external COpenClipp;

/// Scales values of Source image according to the given input and output ranges
function ocipLutScale(Source, Dest: TOcipImage; SrcMin, SrcMax, DstMin, DstMax: Single): TOcipError; cdecl; external COpenClipp;



// Morphology --------------------------------------------------------------------------------------
function ocipPrepareMorphology(Image: TOcipImage): TOcipError; cdecl; external COpenClipp;  ///< See ocipPrepareExample
// Single iteration
function ocipErode(Source, Dest: TOcipImage; Width: Integer): TOcipError; cdecl; external COpenClipp;      ///< 1 Iteration
function ocipDilate(Source, Dest: TOcipImage; Width: Integer): TOcipError; cdecl; external COpenClipp;      ///< 1 Iteration
function ocipGradient(Source, Dest, Temp: TOcipImage; Width: Integer): TOcipError; cdecl; external COpenClipp;  ///< Dilate - Erode
// Multiple iterations
function ocipErode2(Source, Dest, Temp: TOcipImage; Iterations: Integer; Width: Integer): TOcipError; cdecl; external COpenClipp;
function ocipDilate2(Source, Dest, Temp: TOcipImage; Iterations: Integer; Width: Integer): TOcipError; cdecl; external COpenClipp;
function ocipOpen(Source, Dest, Temp: TOcipImage; Depth: Integer; Width: Integer): TOcipError; cdecl; external COpenClipp;   ///< Erode then dilate
function ocipClose(Source, Dest, Temp: TOcipImage; Depth: Integer; Width: Integer): TOcipError; cdecl; external COpenClipp;   ///< Dilate then erode
function ocipTopHat(Source, Dest, Temp: TOcipImage; Depth: Integer; Width: Integer): TOcipError; cdecl; external COpenClipp;   ///< Source - Open
function ocipBlackHat(Source, Dest, Temp: TOcipImage; Depth: Integer; Width: Integer): TOcipError; cdecl; external COpenClipp;   ///< Close - Source



// Transformations ---------------------------------------------------------------------------------
function ocipPrepareTransform(Image: TOcipImage): TOcipError; cdecl; external COpenClipp;   ///< See ocipPrepareExample

/// Mirrors the image along X.
/// D(x,y) = D(width - x - 1, y)
function ocipMirrorX(Source, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;

/// Mirrors the image along Y.
/// D(x,y) = D(x, height - y - 1)
function ocipMirrorY(Source, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;

/// Flip : Mirrors the image along X and Y.
/// D(x,y) = D(width - x - 1, height - y - 1)
function ocipFlip(Source, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;

/// Transposes the image.
/// Dest must have a width >= as Source's height and a height >= as Source's width
/// D(x,y) = D(y, x)
function ocipTranspose(Source, Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;

/// Rotates the source image aroud the origin (0,0) and then shifts it.
/// \param Source : Source image
/// \param Dest : Destination image
/// \param Angle : Angle to use for the rotation, in degrees.
/// \param XShift : Shift along horizonltal axis to do after the rotation.
/// \param YShift : Shift along vertical axis to do after the rotation.
/// \param Interpolation : Type of interpolation to use.
///      Available choices are : NearestNeighbour, Linear, Cubic or BestQuality
///      BestQuality will use Cubic.
function ocipRotate(Source, Dest: TOcipImage; Angle, XShift, YShift: Double; Interpolation: TOcipInterpolationType): TOcipError; cdecl; external COpenClipp;

/// Resizes the image.
/// \param Source : Source image
/// \param Dest : Destination image
/// \param Interpolation : Type of interpolation to use.
///      Available choices are : NearestNeighbour, Linear, Cubic or BestQuality
///      BestQuality will use linear when shrinking and Cubic when enlarging.
/// \param KeepRatio : If false, Dest will be filled with the image from source, potentially changing
///      the aspect ratio of the image. \n If true, the aspect ratio of the image will be kept, potentially
///      leaving part of Dest with invalid (unchaged) data to the right or to the bottom.
function ocipResize(Source, Dest: TOcipImage; Interpolation: TOcipInterpolationType; KeepRatio: TOcipBool): TOcipError; cdecl; external COpenClipp;

/// Shearing transformation.
/// \param Source : Source image
/// \param Dest : Destination image
/// \param ShearX : X Shearing coefficient.
/// \param ShearY : Y Shearing coefficient.
/// \param XShift : Shift along horizonltal axis to do after the shearing.
/// \param YShift : Shift along vertical axis to do after the shearing.
/// \param Interpolation : Type of interpolation to use.
///      Available choices are : NearestNeighbour, Linear, Cubic or BestQuality
///      BestQuality will use Cubic.
function ocipShear(Source, Dest: TOcipImage; ShearX, ShearY, XShift, YShift: Double; Interpolation: TOcipInterpolationType): TOcipError; cdecl; external COpenClipp;

/// Remap
/// \param Source : Source image
/// \param MapX : X Map image, must be 1 channel, F32
/// \param MapY : Y Map image, must be 1 channel, F32
/// \param Dest : Destination image
/// \param Interpolation : Type of interpolation to use.
///      Available choices are : NearestNeighbour, Linear, Cubic or BestQuality
///      BestQuality will use Cubic.
function ocipRemap(Source: TOcipImage; MapX, MapY: TOcipImage; Dest: TOcipImage; Interpolation: TOcipInterpolationType): TOcipError; cdecl; external COpenClipp;

/// Sets all values of Dest to value
function ocipSet(Dest: TOcipImage; Value: Single): TOcipError; cdecl; external COpenClipp;



// Filters -----------------------------------------------------------------------------------------
function ocipPrepareFilters(Image: TOcipImage): TOcipError; cdecl; external COpenClipp;  ///< See ocipPrepareExample

/// Gaussian blur filter - with sigma parameter.
/// \param Sigma : Intensity of the filer - Allowed values : 0.01-10
function ocipGaussianBlur(Source, Dest: TOcipImage; Sigma: Single): TOcipError; cdecl; external COpenClipp;

/// Gaussian filter - with width parameter.
/// \param Width : Width of the filter box - Allowed values : 3 or 5
function ocipGauss(Source, Dest: TOcipImage; Width: Integer): TOcipError; cdecl; external COpenClipp;

/// Sharpen filter.
/// \param Width : Width of the filter box - Allowed values : 3
function ocipSharpen(Source, Dest: TOcipImage; Width: Integer): TOcipError; cdecl; external COpenClipp;

/// Smooth filter - or Box filter.
/// \param Width : Width of the filter box - Allowed values : Impair & >=3
function ocipSmooth(Source, Dest: TOcipImage; Width: Integer): TOcipError; cdecl; external COpenClipp;

/// Median filter
/// \param Width : Width of the filter box - Allowed values : 3 or 5
function ocipMedian(Source, Dest: TOcipImage; Width: Integer): TOcipError; cdecl; external COpenClipp;

/// Vertical Sobel filter
/// \param Width : Width of the filter box - Allowed values : 3 or 5
function ocipSobelVert(Source, Dest: TOcipImage; Width: Integer): TOcipError; cdecl; external COpenClipp;

/// Horizontal Sobel filter
/// \param Width : Width of the filter box - Allowed values : 3 or 5
function ocipSobelHoriz(Source, Dest: TOcipImage; Width: Integer): TOcipError; cdecl; external COpenClipp;

/// Cross Sobel filter
/// \param Width : Width of the filter box - Allowed values : 3 or 5
function ocipSobelCross(Source, Dest: TOcipImage; Width: Integer): TOcipError; cdecl; external COpenClipp;

/// Combined Sobel filter
/// Does SobelVert & SobelHoriz and the combines the two with sqrt(V*V + H*H)
/// \param Width : Width of the filter box - Allowed values : 3 or 5
function ocipSobel(Source, Dest: TOcipImage; Width: Integer): TOcipError; cdecl; external COpenClipp;

/// Vertical Prewitt filter
/// \param Width : Width of the filter box - Allowed values : 3 or 5
function ocipPrewittVert(Source, Dest: TOcipImage; Width: Integer): TOcipError; cdecl; external COpenClipp;

/// Horizontal Prewitt filter
/// \param Width : Width of the filter box - Allowed values : 3 or 5
function ocipPrewittHoriz(Source, Dest: TOcipImage; Width: Integer): TOcipError; cdecl; external COpenClipp;

/// Combined Prewitt filter
/// Does PrewittVert & PrewittHoriz and the combines the two with sqrt(V*V + H*H)
/// \param Width : Width of the filter box - Allowed values : 3 or 5
function ocipPrewitt(Source, Dest: TOcipImage; Width: Integer): TOcipError; cdecl; external COpenClipp;

/// Vertical Scharr filter
/// \param Width : Width of the filter box - Allowed values : 3 or 5
function ocipScharrVert(Source, Dest: TOcipImage; Width: Integer): TOcipError; cdecl; external COpenClipp;

/// Horizontal Scharr filter
/// \param Width : Width of the filter box - Allowed values : 3 or 5
function ocipScharrHoriz(Source, Dest: TOcipImage; Width: Integer): TOcipError; cdecl; external COpenClipp;

/// Combined Scharr filter
/// Does ScharrVert & ScharrHoriz and the combines the two with sqrt(V*V + H*H)
/// \param Width : Width of the filter box - Allowed values : 3 or 5
function ocipScharr(Source, Dest: TOcipImage; Width: Integer): TOcipError; cdecl; external COpenClipp;

/// Hipass filter
/// \param Width : Width of the filter box - Allowed values : 3 or 5
function ocipHipass(Source, Dest: TOcipImage; Width: Integer): TOcipError; cdecl; external COpenClipp;

/// Laplace filter
/// \param Width : Width of the filter box - Allowed values : 3 or 5
function ocipLaplace(Source, Dest: TOcipImage; Width: Integer): TOcipError; cdecl; external COpenClipp;



// Histogram ---------------------------------------------------------------------------------------
// All Histogram operations are Syncrhonous, meaning they block until the histogram is calculated and set to Histogram
function ocipPrepareHistogram(Image: TOcipImage): TOcipError; cdecl; external COpenClipp;   ///< See ocipPrepareExample

/// Calculates the Histogram of the first channel of the image
/// \param Histogram : Array of 256 elements that will receive the histogram values
function ocipHistogram_1C(Source: TOcipImage; Histogram: PCardinal): TOcipError; cdecl; external COpenClipp;

/// Calculates the Histogram of all channels of the image
/// \param Histogram : Array of 1024 elements that will receive the histogram values
function ocipHistogram_4C(Source: TOcipImage; Histogram: PCardinal): TOcipError; cdecl; external COpenClipp;

/// Calculates the Otsu threshold given an histogram
function ocipOtsuThreshold(Source: TOcipImage; Value: PCardinal): TOcipError; cdecl; external COpenClipp;



// Statistics --------------------------------------------------------------------------------------
// All Statistics operations are Syncrhonous, meaning they block until the value is calculated and set to Result
function ocipPrepareStatistics(var ProgramPtr: TOcipProgram; Image: TOcipImage): TOcipError; cdecl; external COpenClipp;   ///< See ocipPrepareExample2
// Result must point to an array that is at least NbChannels long
function ocipMin(&Program: TOcipProgram; Source: TOcipImage; out Result: Double): TOcipError; cdecl; external COpenClipp;                  ///< Finds the minimum value in the image
function ocipMax(&Program: TOcipProgram; Source: TOcipImage; out Result: Double): TOcipError; cdecl; external COpenClipp;                  ///< Finds the maximum value in the image
function ocipMinAbs(&Program: TOcipProgram; Source: TOcipImage; out Result: Double): TOcipError; cdecl; external COpenClipp;                  ///< Finds the minimum of the absolute of the values in the image
function ocipMaxAbs(&Program: TOcipProgram; Source: TOcipImage; out Result: Double): TOcipError; cdecl; external COpenClipp;                  ///< Finds the maxumum of the absolute of the values in the image
function ocipSum(&Program: TOcipProgram; Source: TOcipImage; out Result: Double): TOcipError; cdecl; external COpenClipp;                  ///< Calculates the sum of all pixel values
function ocipSumSqr(&Program: TOcipProgram; Source: TOcipImage; out Result: Double): TOcipError; cdecl; external COpenClipp;                  ///< Calculates the sum of the sqaure of all pixel values
function ocipMean(&Program: TOcipProgram; Source: TOcipImage; out Result: Double): TOcipError; cdecl; external COpenClipp;                  ///< Calculates the mean value of all pixel values
function ocipMeanSqr(&Program: TOcipProgram; Source: TOcipImage; out Result: Double): TOcipError; cdecl; external COpenClipp;                  ///< Calculates the mean of the square of all pixel values
function ocipStdDev(&Program: TOcipProgram; Source: TOcipImage; out Result: Double): TOcipError; cdecl; external COpenClipp;                  ///< Calculates the standard deviation of all pixel values
function ocipMean_StdDev(&Program: TOcipProgram; Source: TOcipImage; out Mean, StdDev: Double): TOcipError; cdecl; external COpenClipp;   ///< Calculates the standard deviation and mean of all pixel values
// These operate only on the first channel of the image, Result, IndexX and IndexY can point to a single value
function ocipCountNonZero(&Program: TOcipProgram; Source: TOcipImage; out Result: Cardinal): TOcipError; cdecl; external COpenClipp;                              ///< Calculates the number of pixels that have a non zero value
function ocipMinIndx(&Program: TOcipProgram; Source: TOcipImage; out Result: Double; out IndexX, IndexY: Integer): TOcipError; cdecl; external COpenClipp;  ///< Finds the minimum value in the image and the coordinate (index) of the pixel with that value
function ocipMaxIndx(&Program: TOcipProgram; Source: TOcipImage; out Result: Double; out IndexX, IndexY: Integer): TOcipError; cdecl; external COpenClipp;  ///< Finds the maximum value in the image and the coordinate (index) of the pixel with that value
function ocipMinAbsIndx(&Program: TOcipProgram; Source: TOcipImage; out Result: Double; out IndexX, IndexY: Integer): TOcipError; cdecl; external COpenClipp;  ///< Finds the minimum of the absolute values in the image and the coordinate (index) of the pixel with that value
function ocipMaxAbsIndx(&Program: TOcipProgram; Source: TOcipImage; out Result: Double; out IndexX, IndexY: Integer): TOcipError; cdecl; external COpenClipp;  ///< Finds the maximum of the absolute values in the image and the coordinate (index) of the pixel with that value



// Thresholding ------------------------------------------------------------------------------------
function ocipPrepareThresholding(Image: TOcipImage): TOcipError; cdecl; external COpenClipp; ///< See ocipPrepareExample

type
  TCompareOperation = (
    LT,
    LQ,
    EQ,
    GQ,
    GT
  );

/// D = (S Op Thresh ? value : S)
function ocipThreshold(Source, Dest: TOcipImage; Thresh, Value: Single; Op: TCompareOperation): TOcipError; cdecl; external COpenClipp;

/// D = (S > threshGT ? valueHigher : (S < threshLT ? valueLower : S) )
function ocipThresholdGTLT(Source, Dest: TOcipImage; ThreshLT, ValueLower, ThreshGT, ValueHigher: Single): TOcipError; cdecl; external COpenClipp;

/// D = (S1 Op S2 ? S1 : S2)
function ocipThreshold_Img(Source1, Source2, Dest: TOcipImage; Op: TCompareOperation): TOcipError; cdecl; external COpenClipp;

/// D = (S Op S2) - D will be 0 or 255
/// Dest must be U8 and 1 channel
function ocipCompare(Source1, Source2, Dest: TOcipImage; Op: TCompareOperation): TOcipError; cdecl; external COpenClipp;

/// D = (S1 Op V) - D will be 0 or 255
/// Dest must be U8 and 1 channel
function ocipCompareC(Source, Dest: TOcipImage; Value: Single; Op: TCompareOperation): TOcipError; cdecl; external COpenClipp;



// Blobs -------------------------------------------------------------------------------------------
// All Blob operations are Syncrhonous, meaning they block until the computation is complete but
// no read operation is issued. ocipReadImage() must be called to transfer the result into host memory.
function ocipPrepareBlob(var ProgramPtr: TOcipProgram; Image: TOcipImage): TOcipError; cdecl; external COpenClipp;    ///< See ocipPrepareExample2

/// Compute the blob labels for the given image.
/// PrepareFor() must be called with the same Source image before calling ComputeLabels()
/// All non-zero pixels will be grouped with their neighbours and given a label number
/// After calling, Labels image will contain the label values for each pixel,
/// and -1 (or 0xffffffff) for pixels that were 0
/// \param Source : The image to analyze
/// \param Labels : must be a 32b integer image
/// \param ConnectType : Type of pixel connectivity, can be 4 or 8
function ocipComputeLabels(&Program: TOcipProgram; Source: TOcipImage; Labels: TOcipImage; ConnectType: Integer): TOcipError; cdecl; external COpenClipp;

/// Renames the labels to be from 0 to NbLabels-1.
/// \param Labels : must be an image resulting from a previous call to ComputeLabels()
function ocipRenameLabels(&Program: TOcipProgram; Labels: TOcipImage): TOcipError; cdecl; external COpenClipp;



// FFT ---------------------------------------------------------------------------------------------
function ocipPrepareFFT(var ProgramPtr: TOcipProgram; RealImage, ComplexImage: TOcipImage): TOcipError; cdecl; external COpenClipp;   ///< See ocipPrepareExample2

function ocipIsFFTAvailable(): TocipBool; cdecl; external COpenClipp;  ///< Returns true if the library has been compiled with FFT

/// Forward Fast Fourrier Transform.
/// Executes a fast fourrier transform on the given image
/// The size of RealSource is used as the dimention for the transformation.
/// \param Program : The FFT program prepared by ocipPrepareFFT
/// \param RealSource : An image containing a 1 channel image of F32 real values
/// \param ComplexDest : An image that will received the transformed image as complex numbers.
///                      It must be 2 channels of F32 and its width must be >= Width(RealSource)/2+1\n
///                      First channel is Real and second channel is Imaginary
function ocipFFTForward(&Program: TOcipProgram; RealSource, ComplexDest: TOcipImage ): TOcipError; cdecl; external COpenClipp;

/// Inverse (Backward) Fast Fourrier Transform.
/// Executes an inverse fast fourrier transform on the given complex image
/// The size of RealDest is used as the dimention for the transformation.
/// \param Program : The FFT program prepared by ocipPrepareFFT
/// \param ComplexSource : An image containing a 2 channel image of F32 as complex numbers. First channel is Real and second channel is Imaginary.
///                        Its width must be >= Width(RealDest)/2+1.
/// \param RealDest : An image containing a 1 channel image of F32. Will receive the transformed image as real numbers only (no imaginary part).
function ocipFFTInverse(&Program: TOcipProgram; ComplexSource, RealDest: TOcipImage): TOcipError; cdecl; external COpenClipp;



// Integral ----------------------------------------------------------------------------------------
function ocipPrepareIntegral(var ProgramPtr: TOcipProgram; Image: TOcipImage): TOcipError; cdecl; external COpenClipp; ///< See ocipPrepareExample

///< Scans the image and generates the Integral sum into Dest buffer - Dest must be F32 or F64 - 1 channel
function ocipIntegral(&Program: TOcipProgram; Source: TOcipImage; Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;

///< Scans the image and generates the Square Integral sum into Dest buffer - Dest must be F32 or F64 - 1 channel
function ocipSqrIntegral(&Program: TOcipProgram; Source: TOcipImage; Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;



// Image Proximity ---------------------------------------------------------------------------------
// All ImageProximity operations are Syncrhonous, meaning they block until the ImageProximity is calculated and set to the result
// Use only small template images (<=16x16 pixels)
// Will be very slow if big template images are used
// For faster image proximity operations with big template image, use ImageProximityFFT
function ocipPrepareProximity(Image: TOcipImage): TOcipError; cdecl; external COpenClipp;   ///< See ocipPrepareExample

/// Computes normalized Euclidean distance between an image and a template.
function ocipSqrDistance_Norm(Source: TOcipImage; Template: TOcipImage; Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;

/// Computes Euclidean distance between an image and a template.
function ocipSqrDistance(Source: TOcipImage; Template: TOcipImage; Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;

//Computes the sum of the absolute difference between an image and a tamplate
function ocipAbsDistance(Source: TOcipImage; Template: TOcipImage; Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;

//Computes normalized cross-correlation between an image and a template.
function ocipCrossCorr(Source: TOcipImage; Template: TOcipImage; Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;

//Computes normalized the cross-correlation between an image and a tamplate
function ocipCrossCorr_Norm(Source: TOcipImage; Template: TOcipImage; Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;



// Image Proximity accelerated using FFT -----------------------------------------------------------
// If Template is small (<16x16 pixels), the standard versions above may be faster
// FFT operations do not work on images bigger than 16.7Mpixels
function ocipPrepareImageProximityFFT(var ProgramPtr: TOcipProgram; Image: TOcipImage; Template: TOcipImage): TOcipError; cdecl; external COpenClipp;

/// Square different template matching - Images must be F32 - 1 channel
function ocipSqrDistanceFFT(&Program: TOcipProgram; Source: TOcipImage; Template: TOcipImage; Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;

/// Normalized square different template matching - Images must be F32 - 1 channel
function ocipSqrDistanceFFT_Norm(&Program: TOcipProgram; Source: TOcipImage; Template: TOcipImage; Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;

/// Cross correlation template matching - Images must be F32 - 1 channel
function ocipCrossCorrFFT(&Program: TOcipProgram; Source: TOcipImage; Template: TOcipImage; Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;

/// Cross correlation template matching - Images must be F32 - 1 channel
function ocipCrossCorrFFT_Norm(&Program: TOcipProgram; Source: TOcipImage; Template: TOcipImage; Dest: TOcipImage): TOcipError; cdecl; external COpenClipp;

implementation

end.
