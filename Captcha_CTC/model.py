import torch
from torch import nn
from torch.nn import functional as F


class CaptchaModel(nn.Module):
    def __init__(self, num_chars):
        super(CaptchaModel, self).__init__()
        self.conv_1 = nn.Conv2d(3, 128, kernel_size=(3, 3), padding=(1, 1))
        self.max_pool_1 = nn.MaxPool2d(2, 2)
        self.conv_2 = nn.Conv2d(128, 64,
                                kernel_size=(3, 3), padding=(1, 1))
        self.max_pool_2 = nn.MaxPool2d(2, 2)

        self.linear_1 = nn.Linear(1152, 64)
        self.dropout = nn.Dropout(0.2)

        self.gru = nn.GRU(64, 32, bidirectional=True, num_layers=2, dropout=0.25)
        self.out = nn.Linear(64, num_chars + 1)

    def forward(self, images, targets=None):
        bs, c, h, w = images.size()
        # print(bs, c, h, w)
        x = F.relu(self.conv_1(images))
        # print(x.size())
        x = self.max_pool_1(x)
        # print(x.size())
        x = F.relu(self.conv_2(x))
        # print(x.size())
        x = self.max_pool_2(x)  # 1, 64, 18, 75
        # print(x.size())
        x = x.permute(0, 3, 1, 2)
        # print(x.size())
        x = x.view(bs, x.size(1), -1)
        # print(x.size())
        x = self.linear_1(x)
        x = self.dropout(x)
        # print(x.size())
        x, _ = self.gru(x)
        # print(x.size())
        x = self.out(x)
        # print(x.size())
        x = x.permute(1, 0, 2)
        # print(x.size())
        if targets is not None:
            log_softmax_values = F.log_softmax(x, 2)
            input_lengths = torch.full(
                size=(bs,),
                dtype=torch.int32,
                fill_value=log_softmax_values.size(0)
            )
            # print(input_lengths)
            output_lengths = torch.full(
                size=(bs,),
                fill_value=targets.size(1),
                dtype=torch.int32
            )
            # print(output_lengths)
            loss = nn.CTCLoss(blank=0)(
                log_softmax_values, targets, input_lengths, output_lengths
            )
            return x, loss
        return x, None

    if __name__ == '__main__':
        cm = CaptchaModel(19)
        img = torch.rand(5, 3, 75, 300)
        target = torch.randint(1, 20, (5, 5))
        x, loss = cm(img, target)
