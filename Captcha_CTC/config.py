DATA_DIR = 'input/captcha_images_v2/'
BATCH_SIZE = 8  # 16
IMAGE_WIDTH = 300
IMAGE_HEIGHT = 75
NUM_WORKERS = 4
EPOCHS = 200
DEVICE = "cpu"  # cuda
checkpoint_path= 'checkpoints/current_checkpoint.pt'
best_model_path= 'checkpoints/best_model.pt'
