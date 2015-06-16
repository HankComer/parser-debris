module Parse where
import Reformat


parse precs = translate . rearrange precs